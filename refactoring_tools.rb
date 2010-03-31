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
    def replace_in_tree(s,*t)
        t.collect { |x|
            if x.is_a? Array
                replace_in_tree(s,*x)
            else
                x.gsub(/(\G|[^\\])[\\$](\d)/) { "#{$1}#{s[$2.to_i-1+@hidden_patterns]}" }
            end
        }
    end
    def parse(*cs)
        #result = begin ($parser ||= RubyParser.new).process cs.flatten.join; rescue Object; nil; end
        result = begin RubyParser.new.process cs.flatten.join; rescue Object; nil; end
        print "#{cs}  -->  #{result}\n" if $echo
        result
    end  
    def concat_code(*cs)
        #$echo = cs.join.include? '>'
        pcs = {} 
        cs = cs.collect { |c| (c.is_a? Array) ? concat_code(*c) : c }
        cs.each { |c| pcs[c] = parse c }
        safe_cs = cs.collect { |c| (c =~ /^ *[@$]?[a-z_0-9]+ *$/i) ? c : (pcs[c] && pcs[c] == parse("(#{c})")) ? "(#{c})" : c }
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
    def top_op(pt)
        pt and (pt[0] == :call) ? pt[2] : pt[0]
    end
    def set_top_op(pt,op)
        fail unless pt
        if pt[0] == :call
            if op
                pt[2] = op
            else
                fail #can't nil a call
            end
        else
            if op
                pt[0] = op
            else
                pt.shift # removing a not
            end
        end
    end
    def args(pt)
        pt and (pt[0] == :call) ? [pt[1]]+pt[3..-1] : pt[1..-1]
    end
    Converse = {
        :'=='  => :'!=',
        :'=~'  => :'!~',
        :'>'   => :'<=',
        :'<'   => :'>=',
        :not   => nil,
        :'!'   => nil,
        :true  => :false
    }
    Converse.update(Converse.invert)
    Bool_op = Converse.keys.compact
    def bool?(t)
        Bool_op.include?(top_op(t)) or (([:and,:or].include?(top_op(t)) and bool?(t[1]) and bool?(t[2])))
    end
    def bool(*cs)
        bool?(parse(cs)) ? cs : ['!!',cs]
    end
    def conditional?(*cs)
        [:if,:while,:until].include?(top_op(parse(concat_code(*cs))))
    end
    def swap_top(cs,t,old_top,new_top)
        goal = t.dup
        set_top_op(goal,new_top)
        s = concat_code(cs)
        cs_flat = s.split(/(#{old_top})/)
        (0...cs_flat.length).each { |i|
            if cs_flat[i] == old_top.to_s
                guess = [(cs_flat[0,i]||[]),new_top.to_s,(cs_flat[(i+1)..-1]||[])]
                return guess if parse(concat_code(guess)) == goal
            end
        }
        nil
    end
    def converse(*cs)
        t = parse concat_code(cs)
        top = top_op(t)
        if !bool?(t)
            ['!',cs]
        elsif [:not,:"!"].include?(top) 
            case cs.first
            when Array       then [converse(*cs.first)] + cs[1..-1]
            when /!(.*)/     then $1
            when /not *(.*)/ then $1
            else fail
            end
        elsif ct = Converse[top]
            swap_top(cs,t,top,ct) || ['!',cs]
        elsif [:and,:or].include? top
            x = swap_top(cs,t,top,([:and,:or]-[top]).first)
            if x and x.length == 3
                [converse(x.first),x[1],converse(x.last)]
            else
                ['!',cs]
            end
        end
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
    def line_bounded?(match)
        match.pre_match =~ /(^|\n) *\z/ && match.post_match =~ /^(\n|\z)/
    end
    def replace_in(text)
        result = text.gsub(@pat) { |matched_text| match_data = Regexp.last_match; line_bounded?(match_data) ? replace_function(matched_text,match_data) : matched_text }
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
    def like(s,desc=nil)
        s = s.
            gsub(/^( *)\(LINES\)/) { |x| "((?:#{$1}\\0.*\n| *\n)*)" }.
            gsub(/\(DEF\)/,'(def .*)')
        super s,desc
    end
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
        #['lib/puppet/network/format.rb']
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


