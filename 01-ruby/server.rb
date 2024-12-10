require 'socket'
require 'thread'

server = TCPServer.new('localhost', 1234)
puts "Chat server is running on localhost:1234"

clients = []
mutex = Mutex.new

Thread.new do
  loop do
    Thread.start(server.accept) do |client|
      mutex.synchronize { clients << client }
      puts "Client connected: #{client.peeraddr.inspect}"

      begin
        loop do
          message = client.gets
          break if message.nil?
          message.chomp!
          timestamp = Time.now.strftime("%H:%M:%S")
          formatted_message = "#{timestamp} - #{message}"
          puts "Received message: #{formatted_message}"
          mutex.synchronize {
            clients.each do |c|
              c.puts(formatted_message) unless c == client
            end
          }
        end
      rescue => e
        puts "Error: #{e.message}"
      ensure
        mutex.synchronize {
          clients.delete(client)
        }
        puts "Client disconnected: #{client.peeraddr.inspect}"
        client.close
      end
    end
  end
end

sleep
