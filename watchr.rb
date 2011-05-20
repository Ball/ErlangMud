

watch('test/.*\.erl') do |m|
   system "erlc -o ebin #{m[0]}"
   system "erl -pa ebin -s mnesia start -s test_runner run -s init stop"
end
watch('src/.*\.erl') do |m|
   system "erlc -o ebin #{m[0]}"
   system "erl -pa ebin -s mnesia start -s test_runner run -s init stop"
end
