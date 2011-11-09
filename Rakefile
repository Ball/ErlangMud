require 'rake/clean'
ENV['PATH'] = "#{ENV['PATH']};\"c:\\Program Files (x86)\\erl5.8.2\\bin\""

INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
TST = FileList['test/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
TEST_OBJ = SRC.pathmap("%{src,ebin}X.beam") + TST.pathmap("%{test,ebin}X.beam")

CLEAN.include("ebin/*.beam")

directory 'ebin'

#rule ".beam" => ".erl" do |t|
# erl_file = ""
# if File.exists?(t.source.sub(/^ebin/,'src'))
#   erl_file = t.source.sub(/^ebin/,'src')
# else
#   erl_file = t.source.sub(/^ebin/,'test')
# end
# sh "erlc -o ebin #{erl_file}"
#end

#rule ".beam" => proc{|tn| tn.sub(/\.beam$/, ".erl").sub(/^ebin/,'src')} do |t|
#  sh "erlc -o ebin #{t.source}"
#end

task :thing do
  puts TEST_OBJ.join(", ")
end

#task :compile => ['ebin'] + OBJ
desc "builds the erlang files"
task :compile => SRC+TST do
  (SRC + TST).each do |t|
    sh "erlc -o ebin #{t}"
  end
end

task :default => :compile

#task :db => :compile do
#  sh "erl -noshell -pa ebin -s mnesia start -s make_db_work init -s init stop"
#end

desc "opens the shell"
task :shell => :compile do
  sh "erl -pa ebin -s mnesia start"
end

desc "runs the test"
task :test => :compile do
  sh "erl -noshell -smp disable -pa ebin -s mnesia start -s test_runner run -s init stop"
end

