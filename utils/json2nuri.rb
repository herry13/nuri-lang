#!/usr/bin/env ruby

require 'json'

usage = "usage: json2nuri.rb <json-file>"

module Nuri
  def self.string_of_basic_value(v)
    case v
    when String
      if v =~ /^\$[a-zA-Z]/
        v[1..v.length]
      else
        v.inspect
      end
    when Array
      buffer = '['
      buffer << v.map { |item| Nuri::string_of_basic_value(item) }.join(',')
      buffer << ']'
    when NilClass
      'null'
    else
      v.to_s
    end
  end
end

class Array
  def to_nuri_constraints(buffer = '', tab = '')
    case self[0]
    when 'and'
      buffer << "{\n"
      self[1, length].reverse.each do |c|
        raise "invalid constraints #{c.inspect}" unless c.is_a?(Array)
        c.to_nuri_constraints(buffer, "#{tab}  ")
      end
      buffer << "#{tab[2,tab.length]}}\n"
    when 'or'
      buffer << "(\n"
      self[1, length].reverse.each do |c|
        raise "invalid constraints #{c.inspect}" unless c.is_a?(Array)
        c.to_nuri_constraints(buffer, "#{tab}  ")
      end
      buffer << "#{tab[2,tab.length]})\n"
    when '=', '!=', 'in', '>', '>=', '<', '<='
      buffer << "#{tab}#{self[1]} #{self[0]} #{Nuri::string_of_basic_value(self[2])};\n"
    when 'imply'
      buffer << "#{tab}if "
      self[1].to_nuri_constraints(buffer, "#{tab}  ")
      buffer << "#{tab}then "
      self[2].to_nuri_constraints(buffer, "#{tab}  ")
    when 'not'
      buffer "#{tab}not "
      self[1].to_nuri_constraints(buffer, "#{tab}  ")
    end
    buffer
  end

  def to_nuri_effects(buffer = '', tab = '')
    buffer << "{\n"
    each do |effect|
      operator, left, right = effect
      if operator == '='
        # left-hand side
        buffer << "#{tab}  #{left} = "

        # right-hand side
        buffer << "#{Nuri::string_of_basic_value(right)};\n"

      else
        raise 'invalid effect: #{effect.inspect}'
      end
    end
    buffer << "#{tab}}\n"
    buffer
  end
end

class Hash
  def to_nuri
    json_to_nuri('')
  end

  protected

  def json_to_nuri_action_parameters(buffer)
    if length > 0
      buffer << ' ('
      buffer << map { |k, v| "#{k}:#{v}"}.join(',')
      buffer << ')'
    end
  end

  def json_to_nuri(buffer, tab = '')
    if has_key?('.type')
      case self['.type']
      when 'action'
        # parameters
        self['parameters'].json_to_nuri_action_parameters(buffer)
        buffer << " {\n"

        # cost
        buffer << "#{tab}cost = #{self['cost']};\n"

        # conditions
        buffer << "#{tab}conditions "
        if self['conditions'] == true
          buffer << "{}\n"
        else
          self['conditions'].to_nuri_constraints(buffer, "#{tab}  ")
        end
        
        # effects
        buffer << "#{tab}effects "
        self['effects'].to_nuri_effects(buffer, tab)


        buffer << "#{tab[2,tab.length]}}\n"
        return buffer

      when 'enum'
        buffer << " {\n"
        buffer << self['elements'].map { |i| "#{tab}#{i}" }.join(",\n")
        buffer << "#{tab[2,tab.length]}\n}\n"
        return buffer

      when 'schema'
        buffer << ' '
        buffer << "extends #{self['.super']} " if self['.super']
      when 'object'
        buffer << ' '
      else
        buffer << " isa #{self['.type']} "
      end
    end

    buffer << "{\n"
    each do |k, v|
      next if k[0] == '.'
      case k
      when 'import', 'include', '#include'
        buffer << "#{tab}#{k} #{v.inspect};\n"
      when 'global'
        buffer << "#{tab}#{k} "
        v.to_nuri_constraints(buffer, "#{tab}  ")
        buffer << "\n"
      else
        case v
        when Hash
          case v['.type']
          when 'action'
            buffer << "#{tab}def #{k}"
          when 'enum'
            buffer << "#{tab}enum #{k}"
          when 'schema'
            buffer << "#{tab}schema #{k}"
          else
            buffer << "#{tab}#{k}"
          end
          v.json_to_nuri(buffer, "#{tab}  ")
        when /^§[a-zA-Z]/
          buffer << "#{tab}#{k} := #{v[1..v.length]};\n"
        else
          buffer << "#{tab}#{k} = #{Nuri.string_of_basic_value(v)};\n"
        end
      end
    end
    buffer << "#{tab[2,tab.length]}}\n"
  end
end

def json_to_nuri(json)
  nuri = JSON.parse(json).to_nuri
  nuri.strip!
  nuri.chop!
  nuri = nuri[1,nuri.length]
  nuri.strip!
end

if ARGV[0]
  if ARGV[0] == '-i'
    puts json_to_nuri(STDIN.read)
  elsif File.exist?(ARGV[0])
    puts json_to_nuri(File.read(ARGV[0]))
  else
    STDERR.puts "File '#{ARGV[0]}' is not exist!"
  end
else
  STDERR.puts usage
end
