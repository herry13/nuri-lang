require 'json'
require 'fileutils'

module Nuri
  # Ruby wrapper for Nuri language compiler.
  class Compiler
    def nuric
      @nuric ||= "#{home}/share/nuric#{suffix}"
    end

    # :file => file that contains the specification
    # :string => string of specification
    def compile(opts)
      lib = (opts[:lib] ? opts[:lib] : '.')
      if opts[:file]
        compile_file opts[:file], lib
      elsif opts[:string]
        compile_string opts[:string], lib
      end
    rescue
      raise 'Invalid input.'
    end

    # :type => :file | :string
    # :init => initial state specification (filename or string)
    # :goal => goal state specification (filename or string)
    def plan(opts)
      lib = (opts[:lib] ? opts[:lib] : '.')
      case opts[:type]
      when :file
        plan_files opts[:init], opts[:goal], lib
      when :string
        plan_strings opts[:init], opts[:goal], lib
      end
    end

    private

    def compile_file(file, lib)
      IO.popen("export NURI_LIB=\"#{lib}\" && #{nuric} #{file}",
               external_encoding: 'UTF-8') do |io|
        return JSON[io.gets]
      end
    end

    def compile_string(str, lib)
      IO.popen("export NURI_LIB=\"#{lib}\" && #{nuric} -i",
               'r+',
               external_encoding: 'UTF-8') do |io|
        io.puts str
        io.close_write
        return JSON[io.gets]
      end
    end

    def plan_files(init, goal, lib)
      IO.popen("export NURI_LIB=\"#{lib}\" && #{nuric} -p #{init} #{goal}",
               external_encoding: 'UTF-8') do |io|
        return JSON[io.gets]
      end
    end

    def temp_init
      @temp_init ||= '.init.nuri'
    end

    def temp_goal
      @temp_goal ||= '.goal.nuri'
    end

    def plan_strings(init, goal, lib)
      File.open(temp_init, 'w') { |f| f.write(init) }
      File.open(temp_goal, 'w') { |f| f.write(goal) }
      plan_files temp_init, temp_goal, lib
    ensure
      FileUtils.rm_f temp_init
      FileUtils.rm_f temp_goal
    end

    def home
      @home ||= "#{File.dirname(__FILE__)}/.."
    end

    def suffix
      (platform ? ".#{platform}" : '')
    end

    def platform
      @platform ||= case RUBY_PLATFORM
        when /.*linux.*/
          'linux'
        when /.*darwin.*/
          'osx'
        when /.*(cygwin|mswin|mingw|bccwin|wince|emx).*/
          'win'
        else
          ''
        end
    end
  end
end
