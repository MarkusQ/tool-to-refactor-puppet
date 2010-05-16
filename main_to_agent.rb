require File.dirname(__FILE__)+'/refactoring_tools'

commit "Change the main spec to an apply spec" do
    commit_message %q{
        This appears to have been left out of the preceeding commit sequence.
    }
    replace_in 'spec/integration/application/main.rb', 'main' => 'agent'
    run "git mv spec/integration/application/main.rb spec/integration/application/apply.rb"
end

