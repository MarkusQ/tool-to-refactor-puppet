class Integer
    def even?
        self & 1 == 0
    end
    def odd?
        self & 1 == 1
    end
end

class String
    def indentation
        self[/^ */].length
    end
    def as_lines
        split(/\n/,-1)
    end
    def indentations
        as_lines.collect { |l| l.indentation }
    end
    def min_indent
        remove_blank_lines.indentations.min || 0
    end
    def unindented
        as_lines.collect { |s| s.lstrip }.join("\n")
    end
    def deindented
        indented(-min_indent,:including_first)
    end
    def reindented(n=4)
        deindented.indented(n,:including_first)
    end
    def indented(n=4,including_first = false)
        if including_first
            as_lines.collect { |s| (n > 0) ? ' '*n+s : s[-n..-1] }.join("\n")
        elsif n > 0
            gsub(/\n/,"\n"+' '*n)
        else
            gsub(/\n.{#{-n}}/,"\n")
        end
    end
    def remove_blank_lines
        as_lines.find_all { |s| s !~ /^ *$/ }.join("\n")
    end
    def remove_leading_blank_lines
        l = as_lines
        l.shift while !l.empty? && l.first.strip.empty?
        l.join("\n")
    end
    def remove_trailing_blank_lines
        l = as_lines
        l.pop   while !l.empty? && l.last.strip.empty?
        l.join("\n")
    end
    def remove_leading_and_trailing_blank_lines
        remove_leading_blank_lines.remove_trailing_blank_lines
    end
    def split_into_opens_closes_and_others(chars_we_care_about='()[]{}<>')
        chars = split('')
        opens, chars = chars.partition { |ch| chars_we_care_about.include?(ch) and '[{(<'.include?(ch) }
        closes,chars = chars.partition { |ch| chars_we_care_about.include?(ch) and ']})>'.include?(ch) }
        return [opens.join,closes.join,chars.join]
    end
    def balanced?(chars)
        # This is simpleminded, but will do for a start.
        opens,closes,others = chars.split_into_opens_closes_and_others(chars)
        # Others are things like '|' and '/' that are ballanced when there are pairs of them.
        count(others).even? and nesting_depth(opens+closes) == 0
    end
    def balanced_quotes?
        balanced?('"') and balanced?("'")
    end
    def without_escaped_characters
        gsub(/\\./,'')
    end
    def nesting_depth(chars="()[]{}")
        # This is simpleminded, but will do for a start.
        s = without_escaped_characters.gsub(/^([^'"]*)('[^']*'|"[^"{#]*")/,'\1').gsub('#{','{').sub(/#.*$/,'')
        # remove single quoted strings and double quoted strings with no interpolation (really neither '#' or '{')
        opens,closes,others = chars.split_into_opens_closes_and_others(chars)
        fail "Can't handle nesting depth for #{others.inspect} yet" unless others.empty?
        s.count(opens) - s.count(closes)
    end
end

module Git
    def self.tracked_files 
        @tracked_files ||= `git ls-files`.
          split("\n").
          collect { |l| l.chomp}
    end
    def self.commit(msg,&block)
        yield
        File.open('refactor_msg.mqr','w') { |f| f.print msg }
        `git commit -a --file refactor_msg.mqr`
        File.delete 'refactor_msg.mqr'
    end
    def self.modified_files
        `git status`.scan(/^#\s*modified:\s*(.*)/).flatten
    end
end

class A_container
    def initialize(*args,&block)
        @properties = Hash.new(nil)
        instance_eval &block
    end
    def method_missing(id,*arg)
        p id if is_a? A_commit
        arg = arg.flatten.compact.first
        @properties[id] = arg if arg
        @properties[id]
    end
end

class A_replacement < A_container
    attr_reader :count
    def initialize(*args,&block)
        @conditions = []
        @count = 0
        @examples = []
        @reasons_to_skip = []
        @hidden_patterns = 0
        @skipped_files = []
        super(*args,&block)
    end
    def skip!(*args)
        @skip = args
    end
    def skip?
        @skip
    end
    def like(s,desc=nil)
        fail "Can't have more than one pattern" if @pat
        s = /#{s.remove_leading_and_trailing_blank_lines.deindented}/ unless s.is_a? Regexp
        @pat = s
        @pattern_description ||= desc || s.source
    end
    def determ(s)
        # Take
        #    TERM(7) to 7
        #    TERM(3+4) to (3+4)
        #    EXP(3+4) to 3+4
        #    EXP(a = 3+4) to (a = 3+4)
        # etc. for TERM,FACTOR,EXPR,STMNT, etc.
        # They may be nested, contain blocks, etc., e.g.
        #    TERM(z.collect { |b| EXP(b*b) }) doesn't need parins
        # Better to make them use a lambda for with and avoid the nesting issues?
        #     instead of
        #            with '\3EXP(TERM(\2)||TERM(\4))'
        #     they'd write
        #            with { |indent,var,exp1,use,exp2| use+expression(term(exp1)+'||'+term(exp2)) }    
        #            with { |indent,var,exp1,use,exp2| use+(exp1.term+'||'+exp2.term).exp }
        #     but we still have to parse the contents over and over.
        #            with { |indent,var,exp1,use,exp2| use+[exp1.term,'||',exp2.term].exp }    
        #            with { |indent,var,exp1,use,exp2| _(use,_(exp1,'||',exp2)) }    
    end
    require "rubygems"
    $LOAD_PATH << "../refactor" << "../refactor/lib"
    require "ruby_parser"
    def parse(*cs)
        result = begin ($parser ||= RubyParser.new).process cs.flatten.join; rescue Object; nil; end
        print "#{cs}  -->  #{result}\n" if $echo
        result
    end
    def concat_code(*cs)
        #$echo = cs.join.include? '>'
        pcs = {}
        cs = cs.collect { |c| (c.is_a? Array) ? concat_code(*c) : c }
        cs.each { |c| pcs[c] = parse c }
        safe_cs = cs.collect { |c| (c =~ /^ *[a-z0-9]+ $/) ? c : pcs[c] ? "(#{c})" : c }
        target = parse safe_cs
        all_cs = 0...cs.length
        all_cs.each { |i|
            # if its a parsable entity and we get a different result omiting the parins, keep them
            if pcs[cs[i]] and parse(all_cs.collect { |j| (j<=i)? cs[j] : safe_cs[j] } ) != target
                cs[i] = safe_cs[i]
            end
        }
        STDIN.readline if $echo
        cs.join
    end
    def replace_in_tree(s,*t)
        t.collect { |x|
            if x.is_a? Array
                replace_in_tree(s,*x)
            else
                x.gsub(/(\G|[^\\])[\\$](\d)/) { "#{$1}#{s[$2.to_i-1+@hidden_patterns]}" }
            end
        }
    end
    def with(*args,&block)
        #p args
        fail "Can't have more than one replacement" if @replacement
        @replacement_description ||= [args.last].join
        @_replacement = args.first
        @_replacement = @_replacement.split(/(\^\d+)/).collect { |s| s.sub(/\^(\d+)/,"\\"+'\1') } if @_replacement =~ /\^\d/
        #@replacement = block || lambda { |*s| @_replacement.gsub(/(\G|[^\\])[\\$](\d)/) { "#{$1}#{s[$2.to_i-1]}" } }
        @replacement = block || lambda { |*s| concat_code(replace_in_tree(s,@_replacement)) }
    end
    def provided(&block)
        @conditions << block
    end
    def context(n)
        @context_before = @context_after = n
    end
    def context_before(n)
        @context_before = n
    end
    def context_after(n)
        @context_after = n
    end
    def skip_files_where(&block)
        @reasons_to_skip << block
    end
    def replace_function(match,match_data)
        captures   = match_data.captures
        new_version = @conditions.all? { |cond| cond.call(*captures) } ? @replacement.call(*captures) : match
        if new_version != match
            @count += 1
            note_possible_example(match,new_version,match_data)
        end
        new_version
    end
    def note_possible_example(*data)
        examples_wanted = examples_to_show || 3
        @examples << stash_example(*data) if @examples.length < examples_wanted or rand(100) == 0
        @examples.delete_at(rand(@examples.length)) if @examples.length > examples_wanted
    end
    def stash_example(*data)
        data
    end
    def format_example(data)
        old,new,match_data = *data
        pre  = @context_before ? [match_data.pre_match. as_lines[-@context_before..-1]].flatten.compact.join("\n") : ''
        post = @context_after  ? [match_data.post_match.as_lines[0...@context_after  ]].flatten.compact.join("\n") : ''
        [
        "The code:",
        (pre+old+post).reindented(4),
        "becomes:",
        (pre+new+post).reindented(4)
        ].join("\n")
    end
    def replace_in(text)
        changes = nil
        while !changes or changes < @count
            changes = @count
            text.gsub!(@pat) { |match| replace_function(match,Regexp.last_match) }
        end
        text
    end
    def apply_to(file_name,text)
        if (reasons = @reasons_to_skip.collect { |pc| pc.call(file_name,text) }.compact).empty?
            replace_in(text)
        else
            #print "Skipped #{file_name} because #{reasons.join(',')}\n"
            @skipped_files << "#{file_name} because #{reasons.join(',')}" unless reasons.join.empty?
            text
        end
    end
    def passes_tests?
        result = (tests || {}).all? { |a,b|
            c = apply_to('test',a.dup)
            c == b || (c==a && b==:unchanged) || (puts "#{title} fails tests; expected \n    #{a.inspect}\n  to become \n    #{b.inspect}\n  but got \n    #{c.inspect}\n") 
        }
        @count,@examples = 0,[]
        result
    end
    def __(n,foos,foo=foos.sub(/s$/,''))
        case n
        when 0; "no #{foos}"
        when 1; "1 #{foo}"
        else    "#{n} #{foos}"
        end
    end
    def compact_if_possible(*lines)
        one_liner = lines.flatten.collect {|line| line.strip}.join(' ').gsub(/ +/,' ')
        (one_liner.length > 60 or one_liner =~ /\n/) ? lines : one_liner
    end
    def to_s
        [
            title,
            (description.remove_leading_and_trailing_blank_lines.deindented if description),
            compact_if_possible(
                "Replaced #{__(@count,'occurances')} of",
                @pattern_description.reindented(4),
                @replacement_description ? 
                    ["with",@replacement_description.reindented(4)] :
                    'with a computed value'
            ),
            (rational.remove_leading_and_trailing_blank_lines.deindented if rational),
            ("#{__(@examples.length,'Examples')}:" unless @examples.empty?),
            @examples.collect { |e| format_example(e) }.join("\n").indented(4,:including_first),
            (["#{__(@skipped_files.length,'files were','file was')} skipped.",@skipped_files.join("\n").reindented(4)] unless @skipped_files.empty?)
        ].flatten.compact.join("\n\n").gsub(/\n\n+/,"\n\n")
    end
end

class A_line_replacement < A_replacement
#    def replace_in(text)
#        text.as_lines.collect { |line| line.sub(@pat) { |match| replace_function(match,Regexp.last_match)}}.join("\n")
#    end
    def replace_in(text)
        result = text.gsub(@pat) { |match| replace_function(match,Regexp.last_match) }
        @examples = @examples.collect { |e|
            (e.length == 2) ? e : (           
                start = e.last.pre_match.as_lines.length - (@context_before || 0)
                start = 0 if start < 0
                len = (@context_before || 0) + 1 + (@context_after || 0)
                #p [start,len,result.length,result.as_lines.length,text.length,text.as_lines.length]
                [text.as_lines[start,len].join("\n"),result.as_lines[start,len].join("\n")] 
                )
        }
        result
    end
    def format_example(data)
        old,new = *data
        ["The code:",old.reindented(4),"becomes:",new.reindented(4)].join("\n")
    end
end

class A_consecutive_line_replacement < A_replacement
    def initialize(*args,&block)
        super
        @hidden_patterns += 1
    end
    def like(s,desc=nil)
        s = s.remove_leading_and_trailing_blank_lines.deindented
        desc ||= s
        s = s.
            gsub(/\\(\d+)/) { |x| "\\#{$1.succ}"}.
            as_lines.
            join('\n\1')
        super /^( *)#{s}/,desc
    end
    def mungle(rep,*prefix)
        case rep
        when nil;   rep
        when Array; prefix+rep.collect { |x| mungle(x) }
        else        prefix.join+rep.gsub(/\n+/,"\n\\\\0")
        end
    end
    def with(rep=nil,desc=nil,&block)
        rep = rep.remove_leading_and_trailing_blank_lines.deindented if rep.is_a? String
        super mungle(rep,'\0'),desc,&block
    end
end

class A_term_replacement < A_replacement
    def initialize(*args,&block)
        super(*args,&block)
        context 1
        # Would like something along the lines of:
        #
        #    provided { |indent,lead_in| lead_in.balanced_quotes? } 
        #
        # But want to catch things like:
        #
        #         rescue Exception
        #-            $LOG.fatal("Problem reading #{filepath}: #{$!}")
        #+            $LOG.fatal("Problem reading #{filepath}: #{$ERROR_INFO}")
        #             exit(1)
        #          end
    end
    def like(s,desc=nil)
        s = s.source if s.is_a? Regexp
        if s =~ /^\w.*\w$/
            super /\b#{s}\b/,(desc || s)
        else
            super /#{s}/,(desc || s)
        end
    end
end

class A_method_replacement < A_consecutive_line_replacement
    # fills in patterns for (METHOD_HEADER), (LINES), etc.
end

class A_targeted_replacement
    def initialize(*args)
        @changes = args.pop
        @files = args
        @desc = Hash.new(0)
    end
    def skip!(*args)
        @skip = args
    end
    def skip?
        @skip
    end
    def passes_tests?
        true
    end
    def apply_to(file_name,text)
        @changes.each { |old,new| text.gsub!(old) { @desc[[file_name,old,new]] += 1;new}} if @files.include? file_name
        text
    end
    def to_s
        @desc.collect { |x| 
            file,old,new,n = *x.flatten
            "Changed #{old.inspect} to #{new.inspect} in #{file}#{" #{n} times." if n > 1}"
        }.join("\n* ")
    end
end

class A_commit
    attr_reader :refactors,:msg
    def initialize(msg,*args,&block)
        @refactors = []
        @validations = []
        @msg = msg
        instance_eval &block
    end
    def replace(*args,&block)
        @refactors << A_replacement.new(*args,&block)
    end
    def replace_lines(*args,&block)
        @refactors << A_line_replacement.new(*args,&block)
    end
    def replace_consecutive_lines(*args,&block)
        @refactors << A_consecutive_line_replacement.new(*args,&block)
    end
    def replace_terms(*args,&block)
        @refactors << A_term_replacement.new(*args,&block)
    end
    def replace_methods(*args,&block)
        @refactors << A_method_replacement.new(*args,&block)
    end
    def replace_in(*args)
        @refactors << A_targeted_replacement.new(*args)
    end
    def excluded_files
        [
            "lib/puppet/parser/parser.rb",                                 # Generated code
            "lib/puppet/util/rdoc/generators/template/puppet/puppet.rb"    # Lots of html
        ]
    end
    def files
        fail unless excluded_files.all? { |f| Git.tracked_files.include? f }
        (Git.tracked_files - excluded_files).grep(/\.rb$/)
    end
    def to_s
        if refactors.length == 1
            "Code smell: #{msg.strip}\n\n#{refactors}\n"
        else
            "Code smell: #{msg.strip}\n\n#{refactors.collect { |r| "* "+r.to_s.indented(2)}.join("\n\n")}\n"
        end
    end
    def do_it
        return puts("!ruby ../refactor/refactor.rb '#{msg}'") if ARGV[0] == 'script'
        return puts("Skipping #{msg}") if @skip or !(ARGV.empty? or ARGV.any? { |s| msg.include? s })
        puts msg
        refactors.reject! { |r| r.skip? }
        return unless refactors.all? { |r| r.passes_tests? }
        #refactors.each { |r| puts '    ',r.to_s }
        unless Git.modified_files.empty?
            puts "The following files have been modifiied\n    #{Git.modified_files.join("\n    ")}\nPlease commit or revert and then try again."
            abort
        end
        # object if there are uncommitted changes
        Git.commit(self) {
            files.each { |file|
                text = refactors.inject(File.read(file)) { |text,refactor| 
                    # puts "#{file} #{refactor.title || refactor.pattern_description}"; 
                    refactor.apply_to(file,text) 
                    }
                File.open(file,'w') { |f| f.print text }
                unless @validations.all? { |v| v.call }
                    print "Changing #{file} broke it."
                    abort
                end
            }
        if false
            `rake spec &> rfrs.out`
            puts File.readlines('rfrs.out').last
        end
        }
    end
    def skip!(*args)
        @skip = true
    end
    def validate_each_file(&block)
        @validations << block
    end
end

def commit(*args,&block)
    A_commit.new(*args,&block).do_it
end

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
    replace_lines {
        like /^(.*?) +$/
        with '\1'
        title "Eliminate trailing spaces."
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
        like '(.*)" *[+] ([$@]?[\w_0-9.:]+?)(.to_s\b)?(?! *[*(%\w_0-9.:{\[])'
        with '\1#{\2}"'
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
        like '"([^"\n]*%s[^"\n]*)" *% *(.+?)(?=$| *\b(do|if|while|until|unless|#)\b)'
        with { |format,args| '"'+@parsed_format.zip(@parsed_args.collect {|a| '#{'+a.strip.sub(/\.to_s$/,'')+'}'}+[nil]).flatten.compact.join+'"'+@post }
        provided { |format,args|
            #p [format,args]
            @parsed_format = format.split('%s',-1)
            log = []
            if args =~ /^\[./
                args,@post = args[0,1],args[1..-1]
                args,@post = args+@post[0,1],@post[1..-1] until @post.empty? or args.balanced?("[]")
                args = $1 if args =~ /^\[(.+)\]$/
            else
                args,@post = args[0,1],args[1..-1]
                (log << [args,@post,args.nesting_depth]; args,@post = args+@post[0,1],@post[1..-1]) until @post.empty? || (args+@post[0,1]).nesting_depth < 0 || (args.nesting_depth == 0 && @post=~ /^(,|;|=>)/)
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
            #log.each { |i| p i } unless result
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
            '"Foo %s" % name # use Foo a la #375'                       => '"Foo #{name}" # use Foo a la #375'

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
        like '(if .*?) *then$'
        with '\1'
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
        with %q{
            if \2
                return \1
            else
                return \3
            end
        }
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])false
            else
                \2true
            end
        }
        with '\2!^1'
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])true
            else
                \2false
            end
        }
        with '\2!!^1'
    }
    replace_consecutive_lines {
        like %q{
            if ([a-z_]) = (.*)
                (.*[^:])\1
            else
                \3(.*)
            end
        }
        with ['\3',['\2','||','\4']]
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
        with ['\2',['\1','     &&     ','\3']]
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])nil
            else
                \2(true)
            end
        }
        with ['\2',['\1','||','nil']]
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])true
            else
                \2nil
            end
        }
        with ['\2',['\1','||','nil']]
    }
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*[^:])\1
            else
                \2nil
            end
        }
        with ['\2',['\1','||','nil']]
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
        with ['\2',['\1','?','\3',':','nil']]
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
                \2
                \3
            end
        }
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
                \2
            rescue\3
                \4
            end
        }
    }
end

commit "Omit needless checks on defined" do
    replace_terms {
        like /defined\? +([@a-zA-Z_.0-9?=]+)/
        with 'defined?(\1)'
        rational 'This makes detecting subsequent patterns easier.'
    }
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
        like "(@?\w+) += +(.*) unless \1"
        with '\1 ||= \2'
    }
end

commit "Use &&= for dependent initialization" do
    replace_lines {
        like "(@?\w+) += +(.*) if \1"
        with '\1 &&= \2'
    }
end

commit "Don't restate results directly after assignment" do
    replace_consecutive_lines {
        like %q{
                (@?\w+)( +\|*= .+)
                \1
            end
        }
        with "    $1$2"
        provided {  no_conditionson($1) }
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



