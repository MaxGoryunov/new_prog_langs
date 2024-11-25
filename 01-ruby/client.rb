require 'socket'
require 'thread'

socket = TCPSocket.new('localhost', 1234)
puts "Connected to the chat server on localhost:1234"
running = true

reader_thread = Thread.new do
  while running
    ready = IO.select([socket], nil, nil, 1)
    if ready
      message = socket.gets
      break if message.nil? 
      puts "\n" + message if message  
    end
  end
end

begin
  loop do
    print "Enter your message (type 'exit' to quit): "
    message = gets.chomp
    break if message.downcase == 'exit' 
    socket.puts message
  end
ensure
  running = false       
  reader_thread.join   
  socket.close 
  puts "Connection closed."
end
