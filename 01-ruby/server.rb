require 'socket'
require 'thread'

server = TCPServer.new('localhost', 1234)
puts "Address: localhost:1234"

clients = []
client_ids = {}
mutex = Mutex.new
client_counter = 0

Thread.new do
  loop do
    Thread.start(server.accept) do |client|
      mutex.synchronize do
        client_counter += 1
        clients << client
        client_ids[client] = client_counter
      end
      puts "Client connected: #{client.peeraddr.inspect} with ID: #{client_counter}"

      begin
        loop do
          message = client.gets
          break if message.nil?
          message.chomp!
          timestamp = Time.now.strftime("%H:%M:%S")
          client_id = client_ids[client]
          formatted_message = "#{timestamp} - [Client #{client_id}] #{message}"
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
          client_ids.delete(client)
        }
        puts "Client disconnected: #{client.peeraddr.inspect}"
        client.close
      end
    end
  end
end

sleep
