def name
  __FILE__.split('.').first
end

def version_file
  "#{File.dirname(__FILE__)}/../VERSION"
end

Gem::Specification.new do |s|
	s.name			= 'nuric'
	s.version		= File.read(version_file).strip
	s.date			= File.atime(version_file).strftime('%Y-%m-%d').to_s
	s.summary		= 'Nuri Language Compiler'
	s.description	= 'A Ruby wrapper of Nuri language compiler'
	s.authors		= ['Herry']
	s.email			= 'herry13@gmail.com'

	s.executables << 'nuric'
	s.files			= `git ls-files`.split("\n").select { |n|
                        if n =~ /^ruby\/.*/
                        !(n =~ /^test\/.*/)
                      }

	s.require_paths = ['lib']
	s.license       = 'BSD'

	s.homepage		= 'https://github.com/herry13/sfp-ruby'
	s.rubyforge_project = 'sfp'

	s.add_dependency 'antlr3', '~> 1.10.0'

	s.add_development_dependency 'rake'
end	
