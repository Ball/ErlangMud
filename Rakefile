require 'rake/clean'
if ENV.has_key?("windir")
  ENV['PATH'] = "#{ENV['PATH']};\"c:\\Program Files (x86)\\erl5.8.2\\bin\""
end

INCLUDE = "include"
ERLC_FLAGS = "-I #{INCLUDE} +warn_unused_vars +warn_unused_import"

def compile(erlfile, beamfile)
  file beamfile => erlfile do
    sh "erlc #{ERLC_FLAGS} -o ebin #{erlfile}"
  end
  task :compile => [beamfile]
end
def compile_dir(dirname)
  FileList["#{dirname}/*.erl"].each do |erlfile|
    beamfile = erlfile.sub(/^src/, "ebin").sub(/^test/,"ebin").sub(/\.erl$/, ".beam")
    compile(erlfile,beamfile)
  end
end
compile_dir("src")
compile_dir("test")

CLEAN.include("ebin/*.beam")

desc "builds the erlang files"
task :compile
task :default => :compile

desc "opens the shell"
task :shell => :compile do
  sh "erl -pa ebin -s mnesia start"
end

desc "runs the test"
task :test => [:compile] do
  sh "erl -noshell -smp disable -pa ebin -s mnesia start -s test_runner run -s init stop"
end

