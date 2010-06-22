require File.dirname(__FILE__)+'/refactoring_tools'

commit "Use the 'root' feature rather than directly checking the uid" do
    commit_message %q{
        Jesse fixed all these but David and others moved them and introduced some more so...
    }
    replace_terms { like "Puppet::Util::SUIDManager.uid == 0"; with  "Puppet.features.root?"}
    replace_terms { like "Puppet::Util::SUIDManager.uid != 0"; with "!Puppet.features.root?"}
    replace_terms { 
        like 'Puppet::Util::SUIDManager.expects\(:uid\).returns 100'
        with 'Puppet.features.expects(:root?).returns false'
    }
    replace_terms { 
        like 'Puppet::Util::SUIDManager.expects\(:uid\).returns 0'
        with 'Puppet.features.expects(:root?).returns true'
    }
end

