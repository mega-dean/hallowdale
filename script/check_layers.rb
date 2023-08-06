require 'json'

def read_file(f)
  contents = File.read(f)
  parsed = JSON.parse(contents)
  parsed.merge("filename" => (File.basename f))
  # puts "read file #{f}, got contents:"
  # puts contents

  # puts "keys:"
  # puts (JSON.parse(contents).keys.join(', '))

  # def is_floors_layer(layer)
  #   layer['type'] == 'objectgroup' && layer['name'] == 'floors'
  # end

  # has_floors_layer = parsed['layers'].any?{|l| is_floors_layer(l)}

  # if has_floors_layer
  #   ok << (File.basename f)
  # # puts "#{File.basename f} - ok"
  # else
  #   missing << (File.basename f)
  #   # puts "#{File.basename f} - MISSING"
  # end
end

def each_layer(json_room, &blk)
  json_room['layers'].each do |layer|
    yield(layer)
  end
end

jsons = Dir['./assets/tiled/rooms/*.json'].map do |file|
  read_file(file)
end

floors = {}

jsons.map do |json|
  each_layer(json) do |layer|
    if layer['type'] == 'objectgroup' && layer['name'] == 'floors'
      floors[json['filename']] = layer['objects'].count
      # floors[json['filename']] = layer['objects'].count
    end
  end
end

with_walls, without_walls = jsons.partition do |json|
  json['layers'].any? do |layer|
    layer['name'] == 'auto:walls'
  end
end

puts "#{without_walls.count} left without walls:"
without_walls.each do |json|
  # if json['filename'].start_with?('outlands_')
    puts "  #{json['filename']}"
  # end
end

# floors.sort_by{|k, v| v}.each do |name, count|
#   puts "#{name}: #{count}"
# end





# TODO
# - find zero-sized rects in object layers
# - could try updating and saving the json for things like "make every world-map layer visible"
