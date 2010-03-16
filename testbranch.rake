require 'digest/md5'
module Git
    class << self
        attr_reader :command,:result
        def try_to(x)
            @command = "git #{x}"
            puts '-'*50,"On #{Git.current_branch}",@command
            #puts "#{Git.current_branch}(#{!!`git log`[/playing with nested scopes/]}): #{@command}"
            @result  = `git #{x} 2>&1`
            #puts `git log | head -n 5`
            puts "'#{@command}' failed!",@result unless successful?
        end
        def array_from(output) 
            output.split("\n").collect { |l| l.chomp.strip }
        end
        def tracked_files 
            array_from(`git ls-files`)
        end
        def commit(msg,&block)
            yield if block_given?
            File.open('refactor_msg.mqr','w') { |f| f.print msg }
            try_to "commit -a --file refactor_msg.mqr"
            File.delete 'refactor_msg.mqr'
            successful?
        end
        def modified_files
            `git status`.scan(/^#\s*modified:\s*(.*)/).flatten
        end
        def unmerged_files
            `git status`.scan(/^#\s*unmerged:\s*(.*)/).flatten
        end
        def diff(*args)
            `git diff #{args.join(' ')}`
        end
        def branches
            array_from(`git branch`)
        end
        def current_branch
            `git branch`.scan(/^\* *(.*)/).flatten.first
        end
        def checkout(branch,base = nil)
            if base
                checkout(base) and delete_branch(branch) and try_to "checkout -b #{branch}"
            else
                try_to "checkout #{branch}"
            end
            successful?
        end
        def delete_branch(branch)
            try_to "branch -D #{branch}" if Git.branches.include? branch
            successful?
        end
        def rename_branch(old,new)
            branch('-m',old,new)
        end
        def method_missing(*args)
            try_to args.join(' ')
            successful?
        end
        def fetch(source,remote,branch)
            delete_branch(branch) and try_to "fetch -f #{source} #{remote}:#{branch}"
            successful?
        end
        def current_commit
            `git log --pretty=oneline -n 1`[/[0-9a-f]+/]
        end
        def commits_since(commit)
            array_from(`git log #{commit}.. --abbrev-commit --pretty=oneline`)
        end
        def conflicts_in?(file)
            !File.readlines(file).grep(/^<<<<<<</).empty?
        end
        def successful?
            !error_message
        end
        def error_message
            case @result
            when /^fatal: (.*)$/; "#{@command} --> #{$1}"
            when /^error: (.*)$/; "#{@command} --> #{$1}"
            when /^(.*failed.*)/; "#{@command} --> #{$1}"
            when /It seems that I cannot create a rebase-apply directory/; @result
            end
        end
    end
end

module Resolutions
    class << self
        def path_as_file_name(path)
            path.gsub('/','-')
        end
        def patch_name(file,n)
            "merge-conflict/resolutions/#{path_as_file_name(file)}/#{n}.patch"
        end
        def branch_name(file,md5)
            "merge-conflict/#{path_as_file_name(file)}/#{md5}"
        end
        def delete_if_present(file)
            File.delete(file) if File.exists? file
        end
        def delete_patch_temp_files(file)
            delete_if_present(file+'.rej')
            delete_if_present(file+'.orig')
        end
        def apply_for(file)
            delete_patch_temp_files(file)
            Dir[patch_name(file,'*')].each { |patch|
                `patch -p 1 < #{patch}`
                delete_patch_temp_files(file)
            }
        end
        def mk_path_to(file)
            unless File.directory?(b = File.dirname(file))
                mk_path_to(b)
                mkdir(b)
            end
        end 
        def needed_for(file,contents,merge_branch) 
            #   produce branches for each of the unresolved merge conflicts for the failed merges
            #   See http://all-thing.net/git-conflict-resolution for why we don't use git-rerere
            conflict_branch = branch_name(file,Digest::MD5.hexdigest(contents.join))
            Git.checkout(conflict_branch,'master')
            mk_path_to(file)
            File.open(file,'w') { |f| f.print contents }
            Git.add(file)
            Git.commit("Unresolved conflicts in #{file}")
            puts "Conflicts merging #{merge_branch}:#{file} written to #{conflict_branch}"
        end
        def look_for_new 
            resolutions = []
            Git.branches.grep(/^merge-conflict\/.*\/[0-9a-f]{32}$/).each { |conflict_branch|
                Git.checkout(conflict_branch)
                file = conflict_branch[%r{/(.*)/},1].gsub('-','/')
                if !Git.conflicts_in?(file)
                    resolutions <<  [file,Git.diff('HEAD^',file)]
                    Git.checkout('master')
                    Git.delete_branch(conflict_branch)
                end
            }

            Git.checkout('master')
            resolutions.each { |file,resolution|
                mk_path_to(patch_name(file,'x'))
                n = 1
                n += 1 while File.exists?(patch_name(file,n))
                File.open(patch_name(file,n),'w') {|f| f.print resolution }
            }
        end
    end
end


desc "Rebuild the 'testng' branch"
task :testbranch do
    test_series = %x{git config --get puppet.testseriesfile}.chomp
    test_series = '.testseries' if test_series.empty?
    sources = Hash.new {|h,k| h[k] = "git://github.com/#{k}/puppet.git"}

    unless Git.modified_files.empty?
        puts "There are uncommited modifications in the following files:",
            Git.modified_files,
            "Please resolve (i.e. commit or revert) and try again."
        abort
    end

    Resolutions.look_for_new
    Git.checkout('testing','master')
    results = []
    conflicts = []
    run_specs = skip_rest = false
    File.readlines(test_series).each do |line|
        line = line.chomp.gsub(/#.*$/,'').strip # ignore trailing spaces, "\n", '#' comments
        case line
        when /^\s*$/ # ignore blank lines
        when /^!(.*)/
            `#{$1}`
            results << "ran #{$1}" + (run_specs ? "\n    #{`rake spec 2>&1 | tail -n 1`}" : '')
        when /(\w+)\s*=\s*(.+)/
            sources[$1] = $2
            # markus= git@github.com:MarkusQ/puppet.git
        when /^ *skip *rest/; skip_rest = true
        when /^ *run *specs/; run_specs = true
        else
            branch_to_merge = line
            result = ''
            if skip_rest
                result = 'skipped'
            else
                begin
                    result = 'succeeded'
                    head_at_start = Git.current_commit
                    try_to_recover = branch_to_merge.gsub!(/^~/,'')
                    if branch_to_merge =~ /(\w+)\s*:\s*(\S+)/
                        # It's a remote branch; fetch it as testing-temp
                        Git.fetch(sources[$1],$2,'testing-temp') and Git.checkout('testing-temp') or fail(Git.error_message)
                    else
                        # It's local; start testing temp off of it
                        Git.checkout('testing-temp',branch_to_merge)
                    end
                    Git.rebase('testing')
                    skipped_commits = []
                    while not Git.successful?
                        if try_to_recover and Git.error_message =~ /Patch failed at \d+ (.*)/
                            commit_name = $1
                            all_better = true
                            Git.unmerged_files.each { |file|
                                begin
                                    puts "Looking for resolultions to conflict(s) in #{file}"
                                    Resolutions.apply_for(file)
                                    if Git.conflicts_in?(file)
                                        Git.add(file)
                                        puts "Fixed conflict(s) in #{file}"
                                    else
                                        all_better = false
                                        conflicts << [file,File.readlines(file),branch_to_merge]
                                        puts "Could not fix conflict(s) in #{file}"
                                    end
                                rescue => e
                                    puts e,e.backtrace
                                end
                            }
                            puts "Tried all the patches"
                            if all_better and Git.unmerged_files.empty?
                                Git.rebase('--continue')
                            else
                                # cache a copy of each file with conflict markers
                                skipped_commits << "Skipping #{commit_name}"
                                Git.rebase('--skip')
                                puts "Skipping #{commit_name}",Git.result
                                result = 'partially succeeded'
                            end
                        else
                            #      * if it fails for some other reason...?
                            # puts '-'*50,"Aborting #{branch_to_merge}",Git.result
                            x = Git.error_message
                            Git.rebase('--abort')
                            `rm -fr .git/rebase-apply/`
                            Git.checkout('testing')
                            Git.delete_branch('testing-temp')
                            fail x
                       end
                    end
                    Git.delete_branch('testing') and Git.rename_branch('testing-temp','testing') or fail Git.error_message
                    result += ":\n    "+Git.commits_since(head_at_start).join("\n    ")+
                        (skipped_commits.empty? ? '' : ["\n  ..skipped",skipped_commits].flatten.join("\n    "))+
                        (run_specs ? "\n  #{`rake spec 2>&1 | tail -n 1`}" : '')
                rescue => e
                    Git.reset('--hard')
                    result = "failed: #{e}"
                end
            end
            results << branch_to_merge+' '+result
        end
    end
    conflicts.each { |file,contents,merge_branch| Resolutions.needed_for(file,contents,merge_branch) }
    Git.checkout('testing')
    puts results
end
