desc "Rebuild the 'testng' branch"
task :testbranch do
    test_series = %x{git config --get puppet.testseriesfile}.chomp
    test_series = '.testseries' if test_series.empty?
    sources = Hash.new {|h,k| h[k] = "git://github.com/#{k}/puppet.git"}

    unless (modified_files = `git status`.scan(/^#\s*modified:\s*(.*)/).flatten).empty?
        puts "There are uncommited modifications in the following files:",
            modified_files,
            "Please resolve (i.e. commit or revert) and try again."
        abort
    end
    
    `git checkout master`
    if `git branch`.split("\n").detect { |l| l =~ /\s+testing$/ }
        `git branch -D testing`
    end
    `git checkout -b testing`
    results = []
    #
    # Instead, 
    #   * make a branch testing-temp-# for each component
    #   * switch to each one in turn and try to rebase it on testing
    #      * if it succeeds, remove testing and rename that branch to testing
    #      * if it fails with a merge conflict(s)
    #           * apply the patches in merge-conflict/<branchname>/*.patch
    #           * remove all .rej files
    #           * if no conflict markers remain, go to step (*) above
    #           * cache a copy of each file with conflict markers
    #           * abort the rebase
    #           * delete the present testing-temp-# branch
    #      * if it fails for some other reason...?
    #   * report which merges succeeded
    #   * checkout master
    #   * produce tagged copies of the unresolved merge conflicts for the failed merges
    #   * commit them
    #   * checkout testing
    #
    File.readlines(test_series).each do |line|
        puts '='*100,line,'='*50
        case line
        when /^\s*$/ # ignore blank lines
        when /^\s*#/ # ignore '#' comments
        when /(\w+)\s*=\s*(.+)/
            sources[$1] = $2
            # markus= git@github.com:MarkusQ/puppet.git
        else
            case line
            when /(\w+)\s*:\s*(\S+)/
                source = sources[$1]
                remote = $2
                branch = "#{$1}/#{remote}"
                puts '='*50
                p [source,remote,branch]
                puts "git pull -f #{source} #{remote}:#{branch} 2>&1"
                puts `git pull -f #{source} #{remote}:#{branch} 2>&1`
                #fatal: Couldn't find remote ref tickets/master/2596
            else
                branch = line.strip
            end
            # Always create a commit for our merge
            puts '='*50
            mm = `git merge --no-ff #{branch} 2>&1`
            puts mm
            puts '='*50
            if mm =~ /Automatic merge failed/ or mm =~ /^fatal:/
                results << "#{branch} failed." << mm
                puts "#{branch} FAILED--attempting reset"
                puts `git reset --hard`
                puts '='*50
            else
                results << "#{branch} succeeded"
            end
        end
    end
    puts '='*100
    puts results
end
