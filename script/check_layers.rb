require 'json'

def read_json_file(f)
  contents = File.read(f)
  parsed = JSON.parse(contents)
end

def read_map_file(f)
  read_json_file(f).merge("filename" => (File.basename f))
end

def each_layer(json_room, &blk)
  json_room['layers'].each do |layer|
    yield(layer)
  end
end

jsons = Dir['./assets/tiled/rooms/*.json'].map do |file|
  read_map_file(file)
end

all_purple_pens = []

with_pp, without_pp = jsons.partition do |json|
  rooms_without_pp = [
    "ac-repair_b.json",
    "computer_f.json",
    #     "forgotten_f.json",
    #     "forgotten_h.json",
    #     "infected_a.json",
    #     "infected_b.json",
    #     "infected_d.json",
    #     "library_a.json",
    #     "library_f.json",
    #     "library_g.json",
    #     "outlands_a.json",
    #     # "trampoline_a.json",
  ]

  if rooms_without_pp.include?(json['filename'])
    true
  else
    json['layers'].any? do |layer|
      if layer['name'] == 'triggers'
        pens = layer['objects'].select{|obj| obj['name'].start_with?('purple-pen:')}
        all_purple_pens += pens
        pens.any?
      end
    end
  end
end

# puts "#{without_pp.count} left without purple-pen:"
# without_pp.each do |json|
#   if json['filename'].start_with?('outlands_')
#     puts "  #{json['filename']}"
#   end
# end

pen_names = all_purple_pens.map{|pen| pen['name'].gsub('purple-pen:', '')}.sort

if pen_names.length != pen_names.uniq.length
  puts "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  puts "!!!!!!!!!! got duplicate pen names !!!!!!!!!!"
  puts "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

  pen_names.each.with_index do |name, i|
    if i != 0
      if name == pen_names[i - 1]
        puts name
      end
    end
  end
end

purple_pens_in_lore = []
lore_file = read_json_file('./config/lore.json')
lore_keys = lore_file.select do |pair|
  if pair[0] =~ /\d.\d\d.\d/ && !pair[1].start_with?('-')
    purple_pens_in_lore << pair[0]
  end
end

unused_purple_pens = purple_pens_in_lore - pen_names

if unused_purple_pens.any?
  puts "\nunused purple pens"
  puts unused_purple_pens.join(', ')
end

pen_counts = {}

jsons.map do |json|
  each_layer(json) do |layer|
    if layer['type'] == 'objectgroup' && layer['name'] == 'triggers'
      pen_counts[json['filename']] = layer['objects'].select{|obj| obj['name'].start_with?('purple-pen')}.count
    end
  end
end

# pen_counts.sort_by{|k, v| v}.each do |name, count|
#   puts "#{name}: #{count}"
# end


with_shadow, without_shadow = jsons.partition do |json|
  json['layers'].any? { |layer| layer['name'] == 'shadow' }
end

if without_shadow.any?
  puts "got #{without_shadow.count} rooms without shadows:"
end

without_shadow.each do |json|
  puts "  #{json['filename']}"
end

