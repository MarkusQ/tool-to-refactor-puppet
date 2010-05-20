require File.dirname(__FILE__)+'/refactoring_tools'

#-------------------------------------------------------------------------------------------------------------------------------------
Line_length_limit = 160
commit "Miscellaneous oddity removal" do
    replace_in "lib/puppet/external/pson/pure/parser.rb",
        'string = self[1].gsub(%r((?:\\\\[\\\\bfnrt"/]|(?:\\\\u(?:[A-Fa-f\\d]{4}))+|\\\\[\x20-\\xff]))n) do |c|' =>
        'string = self[1].gsub(%r{(?:\\\\[\\\\bfnrt"/]|(?:\\\\u(?:[A-Fa-f\\d]{4}))+|\\\\[\x20-\\xff])}n) do |c|'
    replace_in "lib/puppet/agent/runner.rb",
        '"triggered run" %' => '"triggered run"' #Don't try to sprintf into strings with no %-parameters.
    replace_in "lib/puppet/parser/parser_support.rb","lib/puppet/provider/augeas/augeas.rb",
        '"$"' => "'$'"
end

commit "Inconsistent indentation and related formatting issues" do
    replace_terms {
        like /defined\? +([@a-zA-Z_.0-9?=]+)/
        with 'defined?(\1)'
        rational 'This makes detecting subsequent patterns easier.'
    }
    replace_lines {
        like /^(.*?) +$/
        with '\1'
        title "Eliminate trailing spaces."
        skip_files_where { |file_name,text| file_name =~ /test\/ral\/providers\/host\/parsed\.rb/ }
        examples_to_show 0
    }
    replace_lines {
        like /^(\t+)(.*)/
        with { |tabs,content| ' '*(8*tabs.length)+content }
        title "Replace leading tabs with an appropriate number of spaces."
        rational "Tabs are not consistently expanded in all environments."
        examples_to_show 0
        tests ({
            "    spaces" => "    spaces",
            "\ta tab"    => "        a tab"
        })
    }
    replace_consecutive_lines {
        like %q{
            (.*['"] *%)
             +(.*)
        }
        with '\1 \2'
        provided { |indent,first,second|
            first.balanced_quotes? and second.balanced_quotes? and 
            first !~ /^ *#/ and second !~ /^ *#/ and
            (indent+first+second).length < Line_length_limit
        }
        title "Don't arbitrarily wrap on sprintf (%) operator."
        rational "Splitting the line does nothing to aid clarity and hinders further refactorings."
    }    
    replace_consecutive_lines {
        like %q{
            (.*)
             +(.*)
        }
        with '\1 \2'
        provided { |indent,first,second| 
            first.balanced_quotes? and second.balanced_quotes? and 
            (indent+first+second).length < Line_length_limit and
            first.nesting_depth > 0 and (first+second).balanced?('[](){}') and first !~ /"\["/ # This last can be removed when nesting depth respects quotes
        }
        title "Don't break short arrays/parameter list in two."
        tests(
            %q{
                OPEN = /\(/
                CLOSE = /\)/
            } => :unchanged,
            %q{
               puts "to do something objectionable (such as tricking you into overwriting system"
               puts "files if you are running as root)."
            } => :unchanged,
            %q{
                puts "["
                goo.print_to STDOUT; puts "]"
            } => :unchanged
        )
    }
    replace_lines {
        like '(.*\()([^)]*(\([^)]*\))?,)$','lines ending in things like ...(foo, or ...(bar(1,3),'
        with %q{
            \1
                \2
        }
        title "If arguments must wrap, treat them all equally"
        provided { |prefix,arg| prefix !~ / *#/ and prefix.balanced_quotes? and arg.balanced_quotes? }
        tests(
            #'     foo.bar(7,' => 
            '     #foo.bar(7,' => :unchanged
        )
    }
    replace_lines {
        like /^( *)(.*)/
        with { |indent,content|
            i = indent.length
            if content.empty?
                ''
            elsif i == @prior_i
                @prior_result = ' '*@prior_result.indentation+content
            else
                @adjustments.pop while i < @adjustments.last[0]
                d = @adjustments.last[1]
                delta = i+d-@prior_result.indentation
                if delta>4 and content =~ /^#(?![{])/
                    d -= delta
                    content[1,0] = ' '*delta
                elsif delta>4 or ['[','(','{'].include? @prior_result[-1,1]
                    d -= delta-4
                #elsif delta > 4
                #    d -= delta-4
                else
                    case (i+d) % 4
                    when 0: d =d
                    when 1: d-=1
                    when 2:
                        if delta == -2
                            if content =~ /^(else|elsif|rescue|ensure|end|\)|\]|\})/
                                d-=2
                            else
                                d+=2
                            end
                        else
                            d+=2
                            @adjustments.push [i,d]
                        end
                    when 3: d+=1   
                    end
                end
                @prior_i = i
                @prior_result = ' '*(i+d)+content
            end
        }
        rational "
            The present code base is supposed to use four-space indentation.  In some places we failed
            to maintain that standard.  These should be fixed regardless of the 2 vs. 4 space question.
            "
        skip_files_where { |file_name,text| 
            @adjustments = [[0,0]]
            @prior_i = 0
            @prior_result = ''
            text.indentations.all? { |i| i % 4 == 0 } ?  '' : nil
        }
        context 5
        examples_to_show 15
    }
end

commit "Use {} for % notation delimiters wherever practical" do
    replace_in 'spec/unit/parser/lexer.rb',%q{%q["string with ${['an array ',$v2]} in it."]} => %q{%q{"string with ${['an array ',$v2]} in it."}}
    replace_terms { like '%([qQrwWx])\((.*?)\)';     with '%\1{\2}'; provided { |char,      body| !(body.include?('}') || body.include?('('))} }
    replace_terms { like '%([qQrwWx])\[(.*?)\]';     with '%\1{\2}'; provided { |char,      body| !(body.include?('}') || body.include?('['))}; tests('%q[foo[]]' => :unchanged) }
    replace_terms { like '%([qQrwWx])<(.*?)>';       with '%\1{\2}'; provided { |char,      body| !body.include?('}')} }
    replace_terms { like '%([qQrwWx])([^{])(.*?)\2'; with '%\1{\3}'; provided { |char,delim,body| !body.include?('}')}; tests('%r!foo$!' => '%r{foo$}') }
    # Boo hoo:  at.files_matching %r!spec/(unit|integration)/#{m[1]}.rb!
end

commit "English names for special globals rather than line-noise" do
    replace_terms { like '[$][?]';     with '$CHILD_STATUS';      tests('if $? == 0' => 'if $CHILD_STATUS == 0') }
    replace_terms { like '[$][$]';     with '$PID';               }
    replace_terms { like '[$]&';       with '$MATCH';             }
    replace_terms { like '[$]:(?!:)';  with '$LOAD_PATH';         tests('$::var' => :unchanged, '$:.include?' => '$LOAD_PATH.include?') }
    replace_terms { like '[$]!';       with '$ERROR_INFO';        }
    replace_terms { like '^(.*)[$]"';  with '\1$LOADED_FEATURES'
        provided { |prelude| prelude.balanced_quotes? }
        tests('"$"' => :unchanged, '$"' => '$LOADED_FEATURES', 'if $"' => 'if $LOADED_FEATURES')
    }
    %q{
          The code:
              { :acl => "~ ^\/catalog\/([^\/]+)$", :method => :find, :allow => '$1', :authenticated => true },
          becomes:
              { :acl => "~ ^\/catalog\/([^\/]+)$LOADED_FEATURES, :method => :find, :allow => '$1', :authenticated => true },
          The code:
              lambda { @right.newright("~ .rb$")}.should_not raise_error
          becomes:
              lambda { @right.newright("~ .rb$LOADED_FEATURES)}.should_not raise_error
    }
end

commit "Use string interpolation" do
    replace_terms {
        #skip_files_where { |fn,text| @fn = fn; nil }
        like '(.*)" *[+] *([$@]?[\w_0-9.:]+?)(.to_s\b)?(?! *[*(%\w_0-9.:{\[])'
        with '\1#{\2}"'
        #provided { |prefix,suffix| $nnn ||= 0; $nnn += 1; p [@fn,prefix,suffix] if [1,2].include?($nnn); (prefix+'"').balanced_quotes? && [1,2].include?($nnn)}
        provided { |prefix,suffix| (prefix+'"').balanced_quotes? }
    }
    replace_terms {
        like '(.*)" *[+] *"'
        with '\1'
        provided { |first| (first+'"').balanced_quotes? }
    }
    replace_consecutive_lines {
        like %q{
            (.*)(['"]) *[+]
             *(['"])(.*)
        }
        with '\1\4'
        provided { |indent,first,q1,q2,second| 
            (first+q1).balanced_quotes? and 
            (q2+second).balanced_quotes? and
            q1 == q2 and
            (indent+first+second).length < Line_length_limit
        }
        title "Don't use string concatenation to split lines unless they would be very long."
    }
    replace_lines {
        like ' do (.*?) end'
        with ' {\1}'
    }
    replace_terms {
        #like '"([^"\n]*%s[^"\n]*)" *% *([^\]].*?|\[.+\])(?=$| *\#| *\b(do|if|while|until|unless|end|\}|:|,)\b)'
        like '"([^"\n]*%s[^"\n]*)" *% *(.+?)(?=$| *\b(do|if|while|until|unless|#)\b)'
        with { |format,args| '"'+@parsed_format.zip(@parsed_args.collect {|a| '#{'+a.strip.sub(/\.to_s$/,'')+'}'}+[nil]).flatten.compact.join+'"'+@post }
        #provided { |format,args| parse('"'+format+'"') and parse(args) }
        provided { |format,args|
            @parsed_format = format.split('%s',-1)
            log = []
            if args =~ /^\[./
                args,@post = args[0,1],args[1..-1]
                args,@post = args+@post[0,1],@post[1..-1] until @post.empty? or (args.balanced?("[]") and parse(args))
                args = $1 if args =~ /^\[(.+)\]$/
            else
                args,@post = args[0,1],args[1..-1]
                args,@post = args+@post[0,1],@post[1..-1] until @post.empty? || (args+@post[0,1]).nesting_depth < 0 || (args.nesting_depth == 0 && parse("(#{args})") && @post=~ /^ *([,;\#\}]|=>|end)/)
                #@post = ''
                #args,@post = args[0..-2],args[-1,1]+@post until args.balanced_quotes? and args.balanced?('[](){}')
            end
            @parsed_args = args.gsub('join(",','join("@~@').split(',').collect { |a| a.sub(/\.to_s$/,'').gsub(/@~@/,',') }
            i = 0
            while i < @parsed_args.length-1
                if @parsed_args[i].nesting_depth == 0
                    i += 1
                else
                    @parsed_args[i,2] = @parsed_args[i,2].join(',')
                end
            end
            #org = "\"#{format}\" % #{args}"
            result = (@parsed_format.length == @parsed_args.length + 1) && (!@parsed_format.join.include? '%') && @parsed_args.all? {|a| a.nesting_depth == 0 }
            #p [@parsed_format.length,@parsed_args.length,format,@parsed_args,@post,paa,result] unless result
            result
        }
        examples_to_show 20
        tests(
            'msg = "%s access to %s [%s]" % [ (args[:node].nil? ? args[:ip] : "#{args[:node]}(#{args[:ip]})"), name, args[:method] ]' => 
            'msg = "#{(args[:node].nil? ? args[:ip] : "#{args[:node]}(#{args[:ip]})")} access to #{name} [#{args[:method]}]"',
            'raise Err,"type %s exists" % t.to_s if h(:q).include?(t)'  => 'raise Err,"type #{t} exists" if h(:q).include?(t)',
            '"%s[%s]" % [@type, @title]'                                => '"#{@type}[#{@title}]"',
            'args[0].split(/\./).map do |s| "dc=%s"%[s] end.join(",")'  => 'args[0].split(/\./).map do |s| "dc=#{s}" end.join(",")',
            'args[0].split(/\./).map {|s| "dc=%s"%[s]}.join(",")'       => 'args[0].split(/\./).map {|s| "dc=#{s}"}.join(",")',
            'debug(msg + ("(%s)" % label) + (" in %0.2f sec" % value))' => 'debug(msg + ("(#{label})") + (" in %0.2f sec" % value))',
            'option("--#{method}", "-%s" % method.to_s[0,1] ) do'       => 'option("--#{method}", "-#{method.to_s[0,1]}") do',
            '@cs = ("{#{@cst}}") + send("%s_file" % @cst, rp).to_s'     => '@cs = ("{#{@cst}}") + send("#{@cst}_file", rp).to_s',
            'foo "stuff % bar" % list.collect { |x|'                    => :unchanged,
            '"Could not find %s for overriding" % list.collect { |o|'   => :unchanged,
            'info "facts %s" % [::File.basename(file.sub(".rb",""))]'   => 'info "facts #{::File.basename(file.sub(".rb",""))}"',
            '"Foo %s" % name # use Foo a la #375'                       => '"Foo #{name}" # use Foo a la #375',
            %q{method_list = {
                    :intern_method => "from_%s" % name,
                    :intern_multiple_method => "from_multiple_%s" % name,
                    :render_multiple_method => "to_multiple_%s" % name,
                    :render_method => "to_%s" % name
            }} =>
            %q{method_list = {
                    :intern_method => "from_#{name}",
                    :intern_multiple_method => "from_multiple_#{name}",
                    :render_multiple_method => "to_multiple_#{name}",
                    :render_method => "to_#{name}"
            }}
        )
        # For why we use % interpolation, see:
        #    commit 13069eca1a3e2b08f5201462021d83e2e0a81018
        #    Author: Daniel Pittman <daniel@rimspace.net>
        #    Date:   Tue Jul 29 15:52:23 2008 +1000
    } 
end

commit "Line modifiers are preferred to one-line blocks." do
    replace_consecutive_lines {
        like '(while .*?) *do$'
        with '\1'
        examples_to_show 3
        rational "
            The do is unneeded in the block header form and causes problems
            with the block-to-one-line transformation.
        "
    }
    replace_consecutive_lines {
        like '((if|unless) .*?) *then$'
        with '\1'
        provided { |indent,cond_clause,keyword| cond_clause !~ /;$/ } # Don't mess with imbedded bash / puppet code
        examples_to_show 3
        rational "
            The then is unneeded in the block header form and causes problems
            with the block-to-one-line transformation.
        "
    }
    replace_consecutive_lines { 
        like %q{
            ((?:if|unless|while|until) .*)
                (.*)
            end
        }
        with '\2 \1'
        provided { |indent,condition,body|
            condition !~ /\w+ *@?\w+ *=/          and
            body !~ / (if|unless|while|unil) /    and 
            (indent+condition+body).length < Line_length_limit
        }
        rational "
            The one-line form is preferable provided:

                * The condition is not used to assign a variable
                * The body line is not already modified
                * The resulting line is not too long
        "
    }
    # replace_expressions {
    #    like 'if (.*?) if (.*)',
    #    with '\1 and \2',
    #    when { } 
    #}
end

commit "Booleans are first class values." do
    #validate_each_file { s = `spec spec/unit/parser/resource.rb 2>&1`; puts s; s =~ / 0 failures/ } 
    replace_consecutive_lines {
        like %q{
            def (.*)
                begin
                    (.*) = Integer\((.*)\)
                    return \2
                rescue ArgumentError
                    \2 = nil
                end
                if \2 = (.*)
                    return \2
                else
                    return false
                end
            end
        }
        with %q{
            def \1
                Integer(\3) rescue \4 || false
            end
        }
    }
    replace_consecutive_lines {
        like %q{
            return (.*?) if (.*)
            return (.*)
        }
        with ['return',['\2',' ? ','\1',' : ','\3']] 
        provided { |indent,v1,cond,v2| !conditional?(v1) && !conditional?(v2) }
    }
    replace_consecutive_lines {
        like %q{
            return (.*?) unless (.*)
            return (.*)
        }
        with ['return',['\2',' ? ','\3',' : ','\1']] 
        provided { |indent,v1,cond,v2| !conditional(v1) && !conditional?(v2) }
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])false
            else
                \2true
            end
        }
        #with '\2!^1'
        with {|indent,cond,use| indent+concat_code(use,converse(cond)) }
        tests(
            "if a == b\n    plugh false\nelse\n    plugh true\nend\n" => "plugh a != b\n"
        )
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])true
            else
                \2false
            end
        }
        #with '\2!!^1'
        with {|indent,cond,use| indent+concat_code(use,bool(cond)) }
    }
    replace_consecutive_lines {
        like %q{
            if ([a-z_]) = (.*)
                (.*[^:])\1
            else
                \3(.*)
            end
        }
        with ['\3',['\2',' || ','\4']]
        provided { |indent,var,exp1,use,exp2| exp1 !~ / and | or |\|\||&&/ }
    }

    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])\1
            else
                \2false
            end
        }
        with ['\2','\1']
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])(.*)
            else
                \2false
            end
        }
        with ['\2',['\1',' && ','\3']]
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])nil
            else
                \2(true)
            end
        }
        with ['\2',['\1',' || ','nil']]
    }

    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])true
            else
                \2nil
            end
        }
        with ['\2',['\1',' || ','nil']]
    }

    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])\1
            else
                \2nil
            end
        }
        with ['\2',['\1',' || ','nil']]
    }

    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])(.*)
            else
                \2nil
            end
        }
        #with ['\2',[['\1','&&','\3']'||','nil']],
        #tests(
        #    "    if bob\n        sam = foo\n    else        sam = nil\n    end" => "    sam = (bob && foo) || nil"
        #)
        with ['\2',['\1',' ? ','\3',' : ','nil']]
     #   tests(
     #       "    if bob\n        sam = foo\n    else        sam = nil\n    end\n" => "    sam = bob ? foo : nil"
     #   )
    }
    # TODO:
    # def found_file?(path, type = nil)
    #     if data = found_files[path] and ! data_expired?(data[:time])
    #       return false if type and ! data[:stat].send(type)
    #       return true
    #     else
    #       return false
    #     end
    #   end
    #
    #    return false unless defined?(Selinux)
    #    if Selinux.is_selinux_enabled == 1
    #      return true
    #    end
    #    return false
    #  end
    #
    # Not expecting a space before the common clause-prefix allowed this to
    # work but broke with :true & :false -- adding the [^:] fixed that but
    # now this doesn't work again.
    #
    #    def evaluated?
    #      if self.evaluated
    #        true
    #      else
    #        false
    #      end
    #    end
    #
    #      if self.parent
    #        if self.parent == aspect
    #          return true
    #        elsif self.parent.child_of?(aspect)
    #          return true
    #        else
    #          return false
    #        end
    #      else
    #        return false
    #      end
    #
    #
    #-        return true if [:class, :node].include?(name)
    #-        return true if Puppet::Type.type(name)
    #-        return true if known_resource_types.definition(name)
    #-        return false
    #+        if [:class, :node].include?(name)
    #+            return true
    #+        else
    #+            return true if Puppet::Type.type(name)
    #+        end
    #+        return !!(known_resource_types.definition(name))
    #
    # Should be something like:
    #        return [:class, :node].include?(name) || !!Puppet::Type.type(name) || !!known_resource_types.definition(name)
    # 
end

commit "Avoid explicit returns" do
    replace_methods {
        like %q{
            (DEF)
                (LINES)
                return (.*)
            end
        }
        with %q{
            \1
                LINES:\2
                \3
            end
        }
        provided { |indent,header,body,result| parse(result) } # can't elide "return a,b" to "a,b" 'cause it won't parse
    }
end

commit "Avoid unneeded blocks" do
    replace_methods {
        like %q{
            (DEF)
                begin
                    (LINES)
                rescue(.*)
                    (LINES)
                end
            end
        }
        with %q{
            \1
                LINES:\2
            rescue\3
                LINES:\4
            end
        }
    }
end

commit "Omit needless checks on defined" do
    replace_terms {
        like /defined\?\((.+?)\) (?:and|&&) \1( |$)/
        with '\1\2'
        rational %q{
            In code like:

               unless defined? @foo and @foo and bar("baz")

            "defined? @foo and @foo" can safely be replaced with "@foo":

               unless @foo and bar("baz")

            Because:

             * Both evaluate to false/nil when @foo is not defined
             * Both evaluate to @foo when @foo is defined
        }
    }
    replace_terms {
        like /defined\?\((.+?)\) (?:and|&&) ! *\1.nil\?/
        with '!\1.nil?'
        rational %q{
            In code like:

               while defined? @foo and ! @foo.nil? ...

            "defined? @foo and ! @foo.nil?" can safely be replaced with "! @foo.nil?":

               while ! @foo.nil? ...

            Because:

             * Both evaluate to false/nil when @foo is not defined
             * Both evaluate to "! @foo.nil?" when @foo is defined
        }
    }
end

commit "Use ||= for conditional initialization" do
    replace_lines {
        like '([$@]?\w+) += +(.*) +(if +\1.nil\?|if +! *\1|unless +\1|unless +defined\?\(\1\))$'
        with ['\1',' ||= ','\2']
        provided { |var,exp| var !~ /^[A-Z]/ }
        provided { |var,exp| exp != 'true' }
        provided { |var,exp| exp != '@config[param].default' } # A case where we actually care
    }
    # unless defined?
    #    code = [code] unless code.is_a?(Array)
end

commit "Use &&= for dependent initialization" do
    replace_lines {
        like '([$@]?\w+) += +(.*) +(if +\1|unless +\1.nil\?)$'
        with ['\1',' &&= ','\2']
        provided { |var,exp| var !~ /^[A-Z]/ }
    }
end

commit "Don't restate results directly after assignment" do
    replace_consecutive_lines {
        like %q{
                ([$@]?\w+)( +[|&+-]{0,2}= .+)
                \1
            end
        }
        with "    $1$2\nend"
        provided { |indent,var,rest| !conditional?(var+rest) }
    }
end

commit "Avoid needless decorations" do
    #validate_each_file { s = `spec spec/unit/parser/parser.rb 2>&1`; puts s; s =~ / 0 failures/ } 
    replace_terms {
        like /(.*)self\.([a-z_]+)\b(?! *[-&|+*]*=)/
        with '\1\2'
        provided { |prelude,method| method != 'class' and method != 'alias' and method != 'ensure' and prelude !~ /def +$/ }
        tests(
            "self.xyz"      => "xyz",
            "self.class"    => 'self.class',
            'self.x += 7'   => 'self.x += 7',
            'def self.foo'  => 'def self.foo'
        )
        skip! #need to make sure we aren't going to collide with a local variable or parameter -- causes ~900 spec failures
    }
    replace_terms {
        like /(.*)\b([a-z_]+)\(\)/
        with '\1\2'
        provided { |prefix,method| method != 'super'                  } # Because () means no arguments, and nothing means the same arguments we got
        provided { |prefix,method| prefix !~ /\b#{method} *[-+|&]*=/  } # beware things like 'foo = foo()'!
        provided { |prefix,method| prefix.balanced_quotes?            } # We don't want to change things like @parser.parse("tag()")
        provided { |prefix,method| prefix !~ /parser\.parse/          } # Because sometimes we see things like "parser.parse %{tag()}".  *sigh*
        #proc() -> proc
    }
    replace_lines {
        like /^( *)end *#.*/
        with '\1end'
    }
end

commit "Two space indentation" do
    replace_lines {
        like /^( +)(.*$)/
        with { |indent,content| ' '*(indent.length/2)+content }
        provided { |indent,content| !content.empty? }
        context 3
        rational "
            The ruby community almost universally (i.e. everyone but Luke, Markus, and the other eleven people
            who learned ruby in the 1900s) uses two-space indentation.
            "
        skip_files_where { |file_name,text| 
            bad_indents = text.indentations.select { |indent| indent %4 != 0 }
            "it has lines with unexpected indentation (#{bad_indents.join(",")})" unless bad_indents.empty?
        }
    }
end

__END__

error handling
preserve stack traces
output to $stderr

links = links.intern if links.is_a? String --> links = links.to_sym

blah hash[:x] if hash.has_key?[:x]; hash.delete(:x)

1.9 compatibility (when :/then)

#!/usr/bin/env ruby   to #!/usr/bin/env spec    in specs

Why not @compiler.send(:evaluate_ast_node) --> @compiler.evaluate_ast_node?  Because it's how we call private methods when we aern't supposed to, that's why.



