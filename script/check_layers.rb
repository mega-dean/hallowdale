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

with_pp, without_pp = jsons.partition do |json|
  rooms_without_pp = [
    "ac-repair_b.json",
    "basement_a.json",
    "city_a.json",
    "computer_a.json",
    "computer_f.json",
    "computer_h.json",
    "forgotten_a.json",
    "forgotten_f.json",
    "forgotten_h.json",
    "infected_a.json",
    "infected_b.json",
    "infected_d.json",
    "library_a.json",
    "library_f.json",
    "library_g.json",
    "outlands_a.json",
    "trampoline_a.json",
  ]

  if rooms_without_pp.include?(json['filename'])
    true
  else
    json['layers'].any? do |layer|
      if layer['name'] == 'triggers'
        layer['objects'].any?{|obj| obj['name'].start_with?('purple-pen:')}
      end
    end
  end
end

puts "#{without_pp.count} left without purple-pen:"
without_pp.each do |json|
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
