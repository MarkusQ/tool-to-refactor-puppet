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
        l.shift while not l.empty? and l.first.strip.empty?
        l.join("\n")
    end
    def remove_trailing_blank_lines
        l = as_lines
        l.pop   while not l.empty? and l.last.strip.empty?
        l.join("\n")
    end
    def remove_leading_and_trailing_blank_lines
        remove_leading_blank_lines.remove_trailing_blank_lines
    end
    def balanced?(chars)
        # This is simpleminded, but will do for a start.
        chars = chars.split(//)
        opens, chars = chars.partition { |ch| ['[','{','(','<'].include? ch }
        closes,chars = chars.partition { |ch| [']','}',')','>'].include? ch }
        count(chars.join).even? and count(opens.join)-count(closes.join) == 0
    end
    def balanced_quotes?
        balanced?('"') and balanced?("'")
    end
end

module Git
    def self.tracked_files 
        @tracked_files ||= `git ls-files`.
          split("\n").
          collect { |l| l.chomp}.
          grep(/\.rb$/)
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
        @skipped_files = []
        super(*args,&block)
    end
    def like(s,desc=nil)
        fail "Can't have more than one pattern" if @pat
        s = /#{s.remove_leading_and_trailing_blank_lines.deindented}/ unless s.is_a? Regexp
        @pat = s
        @pattern_description ||= desc || s.source
    end
    def with(*args,&block)
        fail "Can't have more than one replacement" if @replacement
        @replacement_description ||= args.last
        @_replacement = args.first
        @replacement = block || lambda { |*s| @_replacement.gsub(/(\G|[^\\])[\\$](\d)/) { "#{$1}#{s[$2.to_i-1]}" } }
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
        result = (tests || {}).all? { |a,b| (c = apply_to('test',a.dup)) == b || (puts "#{title} fails tests; expected #{a.inspect} to become #{b.inspect} but got #{c.inspect}") }
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
    def like(s,desc=nil)
        s = s.remove_leading_and_trailing_blank_lines.deindented
        desc ||= s
        s = s.
            gsub(/\\(\d+)/) { |x| "\\#{$1.succ}"}.
            as_lines.
            join('\n\1')
        super /^( *)#{s}/,desc
    end
    def with(string=nil,desc=nil,&block)
        if string
            super('\0'+string.gsub(/\\n/,"\n\\\\0"),string) { |*s| @_replacement.gsub(/(\G|[^\\])[\\$](\d)/) { $1+s[$2.to_i] } }
        else
            super string,desc,&block
        end
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

class A_commit
    attr_reader :refactors,:msg
    def initialize(msg,*args,&block)
        @refactors = []
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
    def excluded_files
        ["lib/puppet/parser/parser.rb"]
    end
    def files
        Git.tracked_files - excluded_files
    end
    def to_s
        if refactors.length == 1
            "Code smell: #{msg.strip}\n\n#{refactors}\n"
        else
            "Code smell: #{msg.strip}\n\n#{refactors.collect { |r| "* "+r.to_s.indented(2)}.join("\n\n")}\n"
        end
    end
    def do_it
        return puts("Skipping #{msg}") if @skip
        puts msg
        abort unless refactors.all? { |r| r.passes_tests? }
        #refactors.each { |r| puts '    ',r.to_s }
        unless Git.modified_files.empty?
            puts "The following files have been modifiied\n    #{iGit.modified_files.join("\n    ")}\nPlease commit or revert and then try again."
            abort
        end
        # object if there are uncommitted changes
        Git.commit(self) {
            files.each { |file|
                text = refactors.inject(File.read(file)) { |text,refactor| refactor.apply_to(file,text) }
                File.open(file,'w') { |f| f.print text }
            }
        }
    end
    def skip!
        @skip = true
    end
end

def commit(*args,&block)
    A_commit.new(*args,&block).do_it
end

#-------------------------------------------------------------------------------------------------------------------------------------
Line_length_limit = 160
commit "Inconsistant indentation and related formatting issues" do
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
            (indent+first+second).length < Line_length_limit
        }
        title "Don't arbitrarily wrap on sprintf (%) operator."
        rational "Splitting the line does nothing to aid clarity and hinders further refactorings."
        # WTF?
        #          The code:
        #              msg += "triggered run" %
        #              if options[:tags]
        #          becomes:
        #              msg += "triggered run" % if options[:tags]                                                             
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
            (first.count('([{') > first.count(')]}')) and (first+second).balanced?('[](){}')
        }
        title "Don't break short arrays/parameter list in two."
    }
    replace_consecutive_lines {
        like '\(([^)]*(\([^)]*\))?),$'
        with %q{
            (
                \2,
        }
        title "If arguments must wrap, treat them all equally"
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
                if delta>4 and content[0,1] == "#"
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
    replace_terms { like '%([qQrwWx])\((.*?)\)';     with '%\1{\2}'; provided { |char,      body| !body.include?('}')} }
    replace_terms { like '%([qQrwWx])\[(.*?)\]';     with '%\1{\2}'; provided { |char,      body| !body.include?('}')} }
    replace_terms { like '%([qQrwWx])<(.*?)>';       with '%\1{\2}'; provided { |char,      body| !body.include?('}')} }
    replace_terms { like '%([qQrwWx])([^{])(.*?)\2'; with '%\1{\3}'; provided { |char,delim,body| !body.include?('}')}; tests('%r!foo$!' => '%r{foo$}') }
    # Boo hoo:  at.files_matching %r!spec/(unit|integration)/#{m[1]}.rb!
end

commit "English names for special globals rather than line-noise" do
    replace_terms { like '[$][?]'; with '$CHILD_STATUS';     tests({ 'if $? == 0' => 'if $CHILD_STATUS == 0' }) }
    replace_terms { like '[$][$]'; with '$PID';              }
    replace_terms { like '[$]&';   with '$MATCH';            }
    replace_terms { like '[$]:';   with '$LOAD_PATH';        }
    replace_terms { like '[$]!';   with '$ERROR_INFO';       }
    replace_terms { like '[$]"';   with '$LOADED_FEATURES';  }
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
        like '" *[+] ([$@]?[\w_0-9]+)(.to_s\b)?'
        with '#{\1}"'
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
end

commit "Line modifiers are preferred to one-line blocks." do
    replace_consecutive_lines {
        like '(while .*) do *$'
        with '\1'
        examples_to_show 3
        rational "
            The do is unneeded in the block header form and causes problems
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
            condition !~ /\w+ *@?\w+ *=/           and
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
    replace_consecutive_lines {
        like %q{
            if (.*)
                (.*) (true|\1)
            else
                \2 (false|nil)
            end
        }
        #with '\2 !!(\1)'
        with '\2 \1'
    }
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
        like /defined\?\((.+?)\) and \1( |$)/
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
        like /defined\?\((.+?)\) and ! *\1.nil\?/
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

commit "Two space indentation" do
    replace_lines {
        like /^( +)(.*$)/
        with { |indent,content| ' '*(indent.length/2)+content }
        context 3
        rational "
            The ruby community almost universally (i.e. everyone but Luke & Markus) uses two-space indentation.
            "
        skip_files_where { |file_name,text| 
            bad_indents = text.indentations.select { |indent| indent %4 != 0 }
            "it has lines with unexpected indentation (#{bad_indents.join(",")})" unless bad_indents.empty?
        }
    }
end

commit "Avoid needless decorations" do
    replace_terms {
        like /self\.([a-z_]+)(?! *[-&|+*]*=)/
        with '\1'
        provided { |method| method != 'class' }
        tests(
            "self.xyz"      => "xyz",
            "self.class"    => 'self.class',
            'self.x += 7'   => 'self.x += 7',
            'def self.foo'  => 'def self.foo'
        )
    }
    replace_terms {
        like /([a-z_]+)\(\)/
        with '\1'
        #proc() -> proc
        # but beware things like 'foo = foo()'!
    }
    replace_lines {
        like /^(. *)end *#.*/
        with '\1end'
    }
end

__END__

error handling
preserve stack traces
output to $stderr


