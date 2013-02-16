#!/usr/bin/ruby

files = Dir.glob("overlays/*.png")

files.each do |file1|
  files.each do |file2|
    if file1 != file2
      f1base = File.basename(file1, ".png")
      f2base = File.basename(file2, ".png")
      puts "convert Background.jpg -page +0+10 #{file1} -page +111+10 #{file2} -flatten results/#{f1base}_#{f2base}.jpg"
    end
  end
end

