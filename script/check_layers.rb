require 'json'

def read_file(f, ok, missing)
  contents = File.read(f)
  parsed = JSON.parse(contents)
  # puts "read file #{f}, got contents:"
  # puts contents

  # puts "keys:"
  # puts (JSON.parse(contents).keys.join(', '))

  def is_floors_layer(layer)
    layer['type'] == 'objectgroup' && layer['name'] == 'floors'
  end

  has_floors_layer = parsed['layers'].any?{|l| is_floors_layer(l)}

  if has_floors_layer
    ok << (File.basename f)
  # puts "#{File.basename f} - ok"
  else
    missing << (File.basename f)
    # puts "#{File.basename f} - MISSING"
  end
end

ok = []
missing = []

Dir['./assets/tiled/rooms/*.json'].each do |file|
  read_file file, ok, missing
end

puts "done with #{ok.count} / #{(missing.count + ok.count)}"
puts "missing floors:\n#{missing.join("\n")}"



# TODO
# - find zero-sized rects in object layers
# - could try updating and saving the json for things like "make every world-map layer visible"
