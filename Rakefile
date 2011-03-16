require 'rake/clean'
ENV['PATH'] = "#{ENV['PATH']};\"c:\\Program Files (x86)\\erl5.8.2\\bin\""

INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")

CLEAN.include("ebin/*.beam")

directory 'ebin'

rule ".beam" => proc{|tn| tn.sub(/\.beam$/, ".erl").sub(/^ebin/,'src')} do |t|
  sh "erlc -o ebin #{t.source}"
end

task :compile => ['ebin'] + OBJ

task :default => :compile

task :shell => :compile do
  sh "erl -pa ebin -s mnesia start"
end
task :test => :compile do
  sh "erl -noshell -pa ebin -s mnesia start -s erl_start main -s init stop"
end

