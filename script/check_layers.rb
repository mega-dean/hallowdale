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

pen_names = all_purple_pens.map do |pen|
  pen['name']
    .gsub('purple-pen:', '')
    .gsub('+increase-health', '')
    .gsub(/\+(ability|dreamer|weapon|key):.*/, '')
    .gsub(/\/.*/, '')
end.sort

dupe_count = 0

if pen_names.length != pen_names.uniq.length
  puts "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
  puts "!!!!!!!!!! got duplicate pen names !!!!!!!!!!"
  puts "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

  pen_names.each.with_index do |name, i|
    if i != 0
      if name == pen_names[i - 1]
        dupe_count += 1
        puts name
      end
    end
  end
end

if dupe_count > 0
  puts "dupes: #{dupe_count}"
end

purple_pens_in_lore = []
lore_file = read_json_file('./config/lore.json')
lore_keys = lore_file.select do |pair|
  if pair[0] =~ /\d.\d\d.\d/ && !pair[1].start_with?('-')
    purple_pens_in_lore << pair[0]
  end
end

unused_purple_pens = purple_pens_in_lore - pen_names

# if unused_purple_pens.any?
#   puts "\n#{unused_purple_pens.count} unused purple pens:"
#   puts unused_purple_pens.join(', ')
# end

pen_counts = {}

jsons.map do |json|
  each_layer(json) do |layer|
    if layer['type'] == 'objectgroup' && layer['name'] == 'triggers'
      pen_counts[json['filename']] = layer['objects'].select{|obj| obj['name'].start_with?('purple-pen')}.count
    end
  end
end

with_shadow, without_shadow = jsons.partition do |json|
  json['layers'].any? { |layer| layer['name'] == 'shadow' }
end

missing_respawns = jsons.select do |json|
  layer_names = json['layers'].map { |layer| layer['name'] }
  (layer_names.include?('hazard') ||
   layer_names.include?('spikes') ||
   layer_names.include?('acid')) &&
    !layer_names.include?('respawns')
end

with_visible_camera, without_visible_camera = jsons.partition do |json|
  json['layers'].any? { |layer| layer['name'] == 'ref:camera' && layer['visible'] }
end

with_empty_shadow, with_complete_shadow = with_shadow.partition do |json|
  shadow_layer = json['layers'].find{ |l| l['name'] == 'shadow' }
  shadow_layer['data'].all?{|cell| cell == 0} &&
    !json['filename'].start_with?('ac-repair') &&
    !json['filename'] == 'forgotten-test.json'
end

def show_layers(jsons, label)
  if jsons.any?
    puts "got #{jsons.count} rooms #{label}:"

    jsons.each do |json|
      puts "  #{json['filename']}"
    end
  end
end

show_layers(with_visible_camera, "with visible camera")
show_layers(without_shadow, "without shadows")
show_layers(missing_respawns, "missing respawns")
# show_layers(with_complete_shadow, "with complete shadows")
show_layers(with_empty_shadow, "with empty shadows")
