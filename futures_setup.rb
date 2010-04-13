require File.dirname(__FILE__)+'/refactoring_tools'

commit "Fix lingering scope.compiler.class_scope uses" do
    commit_message %q{
        These probably crept in on the backs of merge conflict resolutions.
    }
    replace_terms { like 'scope.compiler.class_scope'; with 'scope.class_scope' }
end

commit "Centralize & normalize the AST#each system" do
    commit_message %q{
        The present system uses a "centralized" each method on the AST branch class
        which should be (but isn't) the ancestor of all AST nodes that contain other
        AST nodes, but it depends on them extending it correctly, which they don't 
        always do, etc., etc.

        This patch moves the method up to the base class, gives it enough smarts that
        it should never need to be overridden, and then removes the ad hoc extensions 
        from the various subclasses that at least tried to do the right thing.

    }
    run "patch -p 1 < centralize_each.patch"
end

commit "Rename AST nodes as Expression nodes" do
    commit_message %q{
        Our AST nodes, which should normally just be an abstract representation of the 
        syntax (showing containment relationships, type infomation, etc.) have grown
        to include semantic information and encapsulate behaviour.  They have become,
        in short, Expressions (aka operator trees) and this commit renames them 
        accordingly.

    }
    replace_lines { like 'AST = Puppet::Parser::AST';   with 'Expression = Puppet::Parser::Expression'  }
    replace_terms { like 'ASTSet';                      with 'SetConstructor (there is no such class)'  }
    replace_terms { like 'ASTArray';                    with 'ArrayConstructor'                         }
    replace_terms { like 'ASTHash';                     with 'HashConstructor'                          }
    replace_terms { like 'AST::';                       with 'Expression::'                             }
    replace_terms { like '::AST';                       with '::Expression'                             }
    replace_lines { like '(describe|it) (.*) AST (.*)'; with '\1 \2 Expression \3'                      }
    replace_terms { like 'AST (object|node)';           with 'Expression node'                          }
    replace_terms { like 'AST tree';                    with 'Expression tree'                          }
    replace_terms { like 'AST subclasses';              with 'Expression node classes'                  }
    replace_terms { like 'AST value';                   with 'Expression'                               }
    replace_terms { like 'AST array';                   with 'Array Expression'                         }
    replace_terms { like 'AST resource';                with 'Resource Expression'                      }
    replace_terms { like 'the AST';                     with 'the Expression tree'                      }
    replace_terms { like 'is_a\?\(AST\)';               with 'is_a? Expression'                         }
    replace_terms { like 'is_a\? AST';                  with 'is_a? Expression'                         }
    replace_terms { like 'FakeAST';                     with 'FakeExpression'                           }
    replace_terms { like 'TestAST';                     with 'TestExpressions'                          }
    replace_terms { like 'ActiveAST';                   with 'ActiveExpression'                         }
    replace_lines { like 'class (.*) < AST';            with 'class \1 < Expression'                    }
    #
    # Move the files
    #
    replace_lines { like '(require.*/ast)/asthash(.*)';  with '\1/hash\2'           }
    replace_lines { like '(require.*/ast)/astarray(.*)'; with '\1/array\2'          }
    replace_lines { like '(require.*/parser)/ast(.*)';  with '\1/expression\2'      }
    run(  
        "git mv test/language/ast.rb              test/language/expression.rb",
        "git mv test/language/ast                 test/language/expression",
        "git mv spec/unit/parser/ast/asthash.rb   spec/unit/parser/ast/hash.rb",
        "git mv spec/unit/parser/ast/astarray.rb  spec/unit/parser/ast/array.rb",
        "git mv spec/unit/parser/ast.rb           spec/unit/parser/expression.rb",
        "git mv spec/unit/parser/ast              spec/unit/parser/expression",
        "git mv lib/puppet/parser/ast/asthash.rb  lib/puppet/parser/ast/hash.rb",
        "git mv lib/puppet/parser/ast/astarray.rb lib/puppet/parser/ast/array.rb",
        "git mv lib/puppet/parser/ast.rb          lib/puppet/parser/expression.rb",
        "git mv lib/puppet/parser/ast             lib/puppet/parser/expression"
    )
    run "cd lib/puppet/parser; racc grammar.ra -o parser.rb"
end

commit "Remove 'settor' flag on Expression nodes" do
    commit_message "'Settor' is an imperative notion and doesn't translate to functional/declarative programs"
    replace_lines { like "@settor = true"; with '' }
    replace_consecutive_lines {
        like %q{
            \# Does this ast object set something\?  If so, it gets evaluated first.
            def self.settor\?(
              .*)*
            end
        }
        with ''
    }
end

commit "Expressions denote values, computation is just a means to this end" do 
    commit_message %q{
        There are many things in the codebase with names based on "evaluate"; to aid in
        understanding the following refactors, those related to Expressions are renamed
        based on the declarative language concept of denotation:
        
        * Expressions denote values, and thus safeevaluate (the external interface to 
          access the value of an Expression) is renamed denotation.
        * Expressions may need to compute thier denotation; thus evaluate (the internal
          interface which does this computation) is renamed compute_denotation.

        This commit should be a rename only and should have NO effect on behaviour.

    }
    #
    #  safeevaluate(options) => denotation
    #
    replace_terms { like 'safeevaluate(\(@?scope\))';                    with 'denotation\1'            }
    replace_terms { like 'safeevaluate( *@?scope\b)';                    with 'denotation\1'            }
    replace       { like ':safeevaluate( *=>)';                          with ':denotation\1'           }
    replace       { like ':safeevaluate(\)\.with\(@scope\))';            with ':denotation\1'           }
    replace_in "spec/unit/resource/type.rb", 
                         "code.expects(:safeevaluate).with @type.subscope" => "code.expects(:denotation).with @type.subscope"
    replace_terms { like ':safeevaluate';                                with ':denotation'             }
    replace_lines { like 'def safeevaluate(\(\*[a-z]+\))';               with 'def denotation\1'        }
    #
    # evaluate
    #
    replace_terms { like 'evaluate(\(@?[a-z]*scope\))';                  with 'compute_denotation\1'    }
    replace_terms { like 'evaluate( @?scope)';                           with 'compute_denotation\1'    }
    replace       { like ':evaluate(\)\.with\(@?scope\))';               with ':compute_denotation\1'   }
    replace_in 'lib/puppet/parser/expression.rb',{
        'def evaluate(*options)' => 'def comute_denotation(*options)',
        'Did not override #evaluate in' => 'Did not override #compute_denotation in',
        'The version of the evaluate method that should be called, because it' => 'Acceses the expressions denotation via a wrapper that memoizes and',
        'return self.evaluate(*options)' => 'compute_denotation(*options)'
    }
end

commit "Expression denotations should be cachable" do
    commit_message %q{
        The denotation of a pure expression should not change over time; thus it
        should be possible to compute the result once and cache (memoize) it.

        Just making the change causes a few test failures, because the tests were
        explicitly depending on the erroneous behaviour, so those most of tests 
        were fixed.  In general the problem was that the older unit tests were 
        constructing an Expression and then repeatedly evaluating it under changing
        conditions, expecting the results to change.
        
        One test, which I do not understand, was not fixed:

            Failure:
            test_storeandcollect(TestScope)
                [./language/scope.rb:235:in `test_storeandcollect'
                 ./language/scope.rb:234:in `each'
                 ./language/scope.rb:234:in `test_storeandcollect'
                 ./language/scope.rb:229:in `times'
                 ./language/scope.rb:229:in `test_storeandcollect'
            Did not find puppet.
            <nil> is not true.
    }
    replace_in 'lib/puppet/parser/expression.rb',{
        'compute_denotation(*options)' => '@denotation ||= compute_denotation(*options)'
    }
    replace_consecutive_lines {
        like %q{
            ast = nil
            param = Expression::Variable.new.:value => "testparam".
            assert_nothing_raised do
              ast = Expression::CaseStatement.new.:test => param, :options => options.
            end
            result = nil
            tests.each do .should, values.
              values.each do .value.
            }
        with %q{
            tests.each do |should, values|
              values.each do |value|
                ast = nil
                param = Expression::Variable.new(:value => "testparam")
                assert_nothing_raised do
                  ast = Expression::CaseStatement.new(:test => param, :options => options)
                end
        }
    }
    run "patch -p 1 < memoize_expression_value_test_fixes.patch"
end

commit "Expression denotations should not depend on scope" do 
    commit_message %q{
        In a declarative language, expressions combine values to produce results in a
        deterministic fashion (3+4=7, regardless of the context).  Expression trees
        should not need to be passed a scope to determine their denotation.

        We aren't quite at that pure state yet, but as expression nodes know their scope
        when they are created (and presently retain that information) we can at least
        pretend that we're pure at the interface and, rather than passing in a scope on
        all calls to evaluate, safeevaluate, evaluate_match, etc. we can omit it and
        use the property in the rare cases where it's needed.  This should also make
        the subsequent finding and removing of those cases much easier.

        To make this a standalone commit we move the scope assignent to the Expression
        node creation which will suffice for tests and be refactored away when we add
        the incarnation phase.  To make refactoring it away easier, we mark all of the
        occurances we add by thowing unneeded parins arround the scope.  This works for
        all but one case (evaluate_code), and for that we add a stub of incarnation.

    }
    #
    #  denotation(options) => denotation
    #
    replace_terms { like 'denotation\(@?scope\)';                      with 'denotation'              }
    replace_terms { like 'denotation @?scope\b';                       with 'denotation'              }
    replace       { like ':denotation *=>';                            with ':denotation =>'          }
    replace       { like ':denotation\)\.with\(@scope\)';              with ':denotation)'            }
    replace_in "spec/unit/resource/type.rb", 
                         "code.expects(:denotation).with @type.subscope" => "code.expects(:denotation)"
    replace_terms { like ':denotation';                                with ':denotation'             }
    replace_lines { like 'def denotation\(\*[a-z]+\)';                 with 'def denotation'          }
    #
    # compute_denotation
    #
    replace_terms { like 'compute_denotation\(@?[a-z]*scope\)';        with 'compute_denotation'      }
    replace_terms { like 'compute_denotation\(\*options\)';            with 'compute_denotation'      }
    replace_terms { like 'compute_denotation @?scope';                 with 'compute_denotation'      }
    replace       { like ':compute_denotation\)\.with\(@?scope\)';     with ':compute_denotation)'    }
    #
    # evaluate_match
    #
    replace       { like 'evaluate_match\((.*), *@?scope\b';           with 'evaluate_match(\1'       }
    replace       { like ':evaluate_match\).with\((.*), @?scope\b';    with ':evaluate_match).with(\1'}
    replace       { 
        like ':evaluate_match\).with \{ \|\*arg\| arg\[0\] == "value" and arg\[1\] == @scope \}'
        with ':evaluate_match).with { |*arg| arg[0] == "value" }'
    }
    replace       { 
        like ':evaluate_match\).with \{ \|\*arg\|(.*?)\}'
        with {|body| ":evaluate_match).with { |*arg|#{body.gsub(/arg\[2\]/,'arg.last')}}" }
    }
    # And what about: evaluate_key, evaluate_classes, evaluate_container, evaluate_ruby_code
    # lib/puppet/parser/expression.rb:    raise Puppet::DevError, "Did not override #evaluate in #{self.class}"
    # test/language/scope.rb:    obj.evaluate config.topscope    
    #
    # Puppet::Parser::Expression::_____.new
    #
    replace { 
        like '(?:Puppet::Parser::Expression|ast)::([A-Z][A-Za-z_]+)\.new([( ]):(?!scope)'
        with 'Puppet::Parser::Expression::\1.new\2:scope => ((@scope)), :'
    }
    replace {
        like '@params = ast::ArrayConstructor.new\(\{\}\)'
        with '@params = Puppet::Parser::Expression::ArrayConstructor.new(:scope => ((@scope)))'
    }
    #
    # Add a stub incarnate
    #
    replace_lines {
        like 'code.denotation if code'
        with 'code.incarnate(scope).denotation if code'
    }
    replace_consecutive_lines {
        like %q{
          # Yield each contained Expression node in turn.  Used mostly by evaluate.
          # This definition means that we don't have to override evaluate

        }
        with %q{
          def incarnate(scope)
            @scope = scope
            each { |child| child.incarnate(scope) }
            self
          end
        
          # Yield each contained Expression node in turn.  Used mostly by incarnate.
          # This definition means that we don't have to override incarnate
        }
    }
end

commit "Remove unsetvar" do
    commit_message %q{
        The "unsetvar" method is many things.  It is, for example, very, very procedural;
        it is also only used in tests.  And in those tests it is only used to show that
        it (unsetvar itseslf) "works," or to unset variables that don't need to be unset.
        And, last but not least, it is removed by this commit.
    }
    replace_methods {
        like %q{
            # Undefine a variable; only used for testing.
            def unsetvar\(var\)
              table = ephemeral\?\(var\) \? @ephemeral : @symtable
              table.delete\(var\) if table.include\?\(var\)
            end
        }
        with ''
    }
    replace_consecutive_lines {
        like %q{
            describe "when unsetting variables" do(
          .*)*?
            end
        }
        with ''
    }
    replace_lines { like 'scope.unsetvar\("output"\)'; with ''}
end

# And unset_ephemeral_var ...?

commit "Replace setvar / lookupvar with futures" do

end

commit "Remove unneeded uses of scope in Expressions" do

end

commit "Switch to full incarnation instead of faking it" do

end
