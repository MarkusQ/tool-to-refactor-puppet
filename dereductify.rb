require File.dirname(__FILE__)+'/refactoring_tools'

commit "Renamed all references to Reductive Labs to Puppet Labs" do
    commit_message %q{
        Find and replace to the rescue...
    }
    replace_terms { like 'reductive'; with 'puppet' }
    replace_terms { like 'Reductive'; with 'Puppet' }
end

