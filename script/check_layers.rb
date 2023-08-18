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

with_platforms, without_platforms = jsons.partition do |json|
  rooms_without_platforms = [
    "ac-repair_b.json",
    "ac-repair_f.json",
    "city_e.json",
    "computer_a.json",
    "computer_g.json",
    "computer_i.json",
    "computer_m.json",
    "computer_f.json",
    "forgotten_d.json",
    "forgotten_h.json",
    "forgotten_g.json",
    "infected_a.json",
    "infected_d.json",
  ]
  if rooms_without_platforms.include?(json['filename'])
    true
  else
    json['layers'].any? do |layer|
      layer['name'] == 'platforms'
    end
  end
end

puts "#{without_platforms.count} left without platforms:"
without_platforms.each do |json|
  # if json['filename'].start_with?('outlands_')
    puts "  #{json['filename']}"
  # end
end

# floors = {}

# jsons.map do |json|
#   each_layer(json) do |layer|
#     if layer['type'] == 'objectgroup' && layer['name'] == 'floors'
#       floors[json['filename']] = layer['objects'].count
#       # floors[json['filename']] = layer['objects'].count
#     end
#   end
# end

# floors.sort_by{|k, v| v}.each do |name, count|
#   puts "#{name}: #{count}"
# end





# TODO
# - find zero-sized rects in object layers
# - could try updating and saving the json for things like "make every world-map layer visible"
