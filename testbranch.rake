desc "Rebuild the 'testng' branch"
task :testbranch do
module Git
    class << self
        def try_to(x)
            @command = "git #{x}"
            puts '-'*50,"On #{Git.current_branch}",@command
            @result  = `git #{x} 2>&1`
            puts `git log | head -n 5`
        end
        def array_from(output) 
            output.split("\n").collect { |l| l.chomp.strip }
        end
        def tracked_files 
            array_from(`git ls-files`)
        end
        def commit(msg,&block)
            yield
            File.open('refactor_msg.mqr','w') { |f| f.print msg }
            try_to "commit -a --file refactor_msg.mqr"
            File.delete 'refactor_msg.mqr'
            successful?
        end
        def modified_files
            `git status`.scan(/^#\s*modified:\s*(.*)/).flatten
        end
        def branches
           array_from(`git branch`)
        end
        def current_branch
            `git branch`.split("\n").grep(/^\*/).join.strip.sub('*','')
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
            try_to "branch -m #{old} #{new}"
            successful?
        end
        def reset_hard
            try_to "reset --hard"
            successful?
        end
        def fetch(source,remote,branch)
            delete_branch(branch) and try_to "fetch -f #{source} #{remote}:#{branch}"
            successful?
        end
        def rebase(base)
            try_to "rebase #{base}"
            successful?
        end
        def successful?
            !error_message
        end
        def error_message
            case @result
            when /^fatal: (.*)$/; "#{@command} --> #{$1} in #{caller[0]}"
            when /^error: (.*)$/; "#{@command} --> #{$1} in #{caller[0]}"
            when /^(.*failed.*)/; "#{@command} --> #{$1} in #{caller[0]}"
            end
        end
    end
end

    test_series = %x{git config --get puppet.testseriesfile}.chomp
    test_series = '.testseries' if test_series.empty?
    sources = Hash.new {|h,k| h[k] = "git://github.com/#{k}/puppet.git"}

    unless Git.modified_files.empty?
        puts "There are uncommited modifications in the following files:",
            Git.modified_files,
            "Please resolve (i.e. commit or revert) and try again."
        abort
    end
    
    Git.checkout('testing','master')
    results = []
    File.readlines(test_series).each do |line|
        line.chomp!
        case line
        when /^\s*$/ # ignore blank lines
        when /^\s*#/ # ignore '#' comments
        when /(\w+)\s*=\s*(.+)/
            sources[$1] = $2
            # markus= git@github.com:MarkusQ/puppet.git
        else
            branch_to_merge = line
            results << branch_to_merge+' '+begin
                if branch_to_merge =~ /(\w+)\s*:\s*(\S+)/
                    # It's a remote branch; fetch it as testing-temp
                    Git.fetch(sources[$1],$2,'testing-temp') and Git.checkout('testing-temp') or fail(Git.error_message)
                else
                    # It's local; start testing temp off of it
                    Git.checkout('testing-temp',branch_to_merge)
                end
                Git.rebase('testing')
                if not Git.successful?
                    puts Git.error_message
                    abort
                    # It is possible that a merge failure will prevent this process from being 
                    # completely automatic. You will have to resolve any such merge failure and
                    # run git rebase --continue. Another option is to bypass the commit that caused 
                    # the merge failure with git rebase --skip. To restore the original
                    # <branch> and remove the .git/rebase-apply working files, use the command git 
                    # rebase --abort instead.
                    #case Git.error_message
                    #
                    #      * if it fails with a merge conflict(s)
                    #           * apply the patches in merge-conflict/<branchname>/*.patch
                    #           * remove all .rej files
                    #           * if no conflict markers remain, go to step (*) above
                    #           * cache a copy of each file with conflict markers
                    #           * abort the rebase
                    #           * delete the present testing-temp-# branch
                    #when /Automatic merge failed/
                    #mm = `git merge --no-ff #{branch} 2>&1`
                    #if mm =~ /Automatic merge failed/ or mm =~ /^fatal:/
                    #    Git.reset_hard
                    #else
                        #      * if it fails for some other reason...?
                    #end
                    fail Git.error_message unless Git.successful?
                end
                Git.delete_branch('testing') and Git.rename_branch('testing-temp','testing') or fail Git.error_message
                abort
                "succeeded"
            rescue => e
                "failed: #{e}"
            end
        end
    end
    puts results
    Git.checkout('master')
    #   * produce tagged copies of the unresolved merge conflicts for the failed merges
    #   See http://all-thing.net/git-conflict-resolution for why we don't use git-rerere
    #   * commit them
    Git.checkout('testing')
end
