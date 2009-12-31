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
        count(chars).even?
    end
    def balanced_quotes?
        balanced?('"') and balanced?("'")
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
            examples_wanted = examples_to_show || 3
            @examples << [match,new_version,match_data] if @examples.length < examples_wanted or rand(100) == 0
            @examples.delete_at(rand(@examples.length)) if @examples.length > examples_wanted
        end
        new_version
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
    def format_example(old,new,match_data)
        pre  = @context_before ? [match_data.pre_match. as_lines[-@context_before..-1]].flatten.compact.join("\n") : ''
        post = @context_after  ? [match_data.post_match.as_lines[0...@context_after  ]].flatten.compact.join("\n") : ''
        [
        "The code:",
        (pre+old+post).reindented(4),
        "\nbecomes:",
        (pre+new+post).reindented(4)
        ].join("\n")
    end
    def to_s
        [
            title,
            (description||'').remove_leading_and_trailing_blank_lines.deindented,
            "Replaced #{(@count > 0) ? @count : 'no'} occurance#{(@count == 1) ? '' : 's'} of",
            @pattern_description.reindented(4),
            @replacement_description ? 
                ["with",@replacement_description.reindented(4)] :
                'with a computed value',
            (rational||'').remove_leading_and_trailing_blank_lines.deindented,
            ("Example#{(@examples.length > 2) ? 's':''}:" unless @examples.empty?),
            @examples.collect { |e| format_example(*e) }.join("\n- - - - - - - -\n").indented(4,:including_first),
            (["#{@skipped_files.length} files were skipped.",@skipped_files.join("\n").reindented(4)] unless @skipped_files.empty?)
        ].flatten.compact.join("\n\n").gsub(/\n\n+/,"\n\n")
    end
end

class A_line_replacement < A_replacement
#    def replace_in(text)
#        text.as_lines.collect { |line| line.sub(@pat) { |match| replace_function(match,Regexp.last_match)}}.join("\n")
#    end
    def replace_in(text)
        text.gsub(@pat) { |match| replace_function(match,Regexp.last_match) }
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
        super /\b#{s}\b/,(desc || s)
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
    def tracked_files 
        @tracked_files ||= `git ls-files`.
          split("\n").
          collect { |l| l.chomp}.
          grep(/\.rb$/)
    end
    def excluded_files
        ["lib/puppet/parser/parser.rb"]
    end
    def files
        tracked_files - excluded_files
    end
    def to_s
        "Code smell: #{msg.strip}\n#{refactors.join("\n#{'-'*50}\n")}\n"
    end
    def do_it
        return puts("Skipping #{msg}") if @skip
        puts msg
        abort unless refactors.all? { |r| r.passes_tests? }
        #refactors.each { |r| puts '    ',r.to_s }
        # object if there are uncommitted changes
        files.each { |file|
            text = refactors.inject(File.read(file)) { |text,refactor| refactor.apply_to(file,text) }
            File.open(file,'w') { |f| f.print text }
        }
        File.open('refactor_msg.mqr','w') { |f| f.print self }
        `git commit -a --file refactor_msg.mqr`
        File.delete 'refactor_msg.mqr'
    end
    def skip!
        @skip = true
    end
end

def commit(*args,&block)
    A_commit.new(*args,&block).do_it
end

unless (modified_files = `git status`.scan(/^#\s*modified:\s*(.*)/).flatten).empty?
    puts "The following files have been modifiied\n    #{modified_files.join("\n    ")}\nPlease commit or revert and then try again."
    abort
end

#-------------------------------------------------------------------------------------------------------------------------------------

commit "Inconsistant indentation" do
    replace_lines {
        like /^(.*?) +$/
        with '\1'
        title "Eliminate trailing spaces."
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
    replace_lines {
        like /^( *)(.*)/
        with { |indent,content|
            @current_line_number += 1
            if content.empty?
                ''
            else
                pause = false
                i = indent.length
                @adjustments.pop while i < @adjustments.last[0]
                d = @adjustments.last[1]
                if (delta=i+d-@prior_result.indentation)>4 and content[0,1] == "#"
                    d -= delta
                    content[1,0] = ' '*delta
                else
                    case (i+d) % 4
                    when 0: d =d
                    when 1: d-=1
                    when 2:
                        if delta == -2
                            if content =~ /^(else|elsif|when|end)/
                                d-=2
                            else
                                d+=2
                            end
                            pause = true
                            #puts "exdent finagle"
                        else
                            d+=2
                            @adjustments.push [i,d]
                        end
                    when 3: d+=1   
                    end
                end
                #print "#{'%3i %3i:' %[i,d]}:#{' '*(i+d)}#{content}"
                #if d != 0 and pause then readline else puts end
                @prior_result = (' '*(i+d))+content
            end
        }
        rational "
            The present code base is supposed to use four-space indentation.  In some places we failed
            to maintain that standard.  These should be fixed regardless of the 2 vs. 4 space question.
            "
        skip_files_where { |file_name,text| 
            @all_indentations = text.indentations
            @current_line_number = 0
            last = 0
            @indentation_deltas = @all_indentations.collect { |i| delta = i-last; last = i; delta }
            @adjustments = [[0,0]]
            @prior_result = ''
            case
            when @all_indentations.all?   { |i| i % 4 == 0 }; ''
            #when (@indentation_deltas - [-2,0,2]).empty?;     "it's already two-space indented"
            else nil
            end
        }
        context 3
    }
end

commit "Use {} for % notation delimiters wherever practical" do
    replace_terms { like '%([qQrwWx])\((.*?)\)';     with '%\1{\2}'; provided { |char,      body| !body.include?('}')} }
    replace_terms { like '%([qQrwWx])\[(.*?)\]';     with '%\1{\2}'; provided { |char,      body| !body.include?('}')} }
    replace_terms { like '%([qQrwWx])<(.*?)>';       with '%\1{\2}'; provided { |char,      body| !body.include?('}')} }
    replace_terms { like '%([qQrwWx])([^{])(.*?)\2'; with '%\1{\3}'; provided { |char,delim,body| !body.include?('}')} }
    # Boo hoo:  at.files_matching %r!spec/(unit|integration)/#{m[1]}.rb!
end

commit "English names for special globals rather than line-noise" do
    replace_terms { like '[$][?]'; with '$CHILD_STATUS';     tests({ 'if $? == 0' => 'if $CHILD_STATUS == 0' }) }
    replace_terms { like '[$][$]'; with '$PID';              }
    replace_terms { like '[$]&';   with '$MATCH';            }
    replace_terms { like '[$]:';   with '$LOAD_PATH';        }
    replace_terms { like '[$]!';   with '$ERROR_INFO';       }
    replace_terms { like '[$]"';   with '$LOADED_FEATURES';  }
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
            (indent+condition+body).length < 120
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
                (.*) true
            else
                \2 false
            end
        }
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
        like /defined? +([@a-zA-Z_.0-9?=]+)/
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

__END__

commit "Avoid needless decorations" do
    refactor_terms {
        like /self\.([a-z_]+)]/
        with '\1'
        provided { |method| method != class }
    }
    refactor_terms {
        like /([a-z_]+)\(\)/
        with '\1'
        #proc() -> proc
    }
    refactor_lines {
        like /^(. *)end *#.*/
        with '\1end
    }'
}

error handling
preserve stack traces
output to $stderr


