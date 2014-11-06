#!/usr/bin/env ruby

require '../lib/nuric.rb'

lib = '../../test'

c = Nuri::Compiler.new
p c.compile(file: './system1-init.nuri', lib: lib)
p c.compile(string: File.read('./system1-init.nuri'), lib: lib)
p c.plan(type: :file,
         init: './system1-init.nuri',
         goal: './system1-goal.nuri',
         lib: lib)

init = File.read('./system1-init.nuri')
goal = File.read('./system1-goal.nuri')
p c.plan(type: :string, init: init, goal: goal, lib: lib)
