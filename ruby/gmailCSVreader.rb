#!/usr/bin/env ruby
#
#  Created by Douglas Richardson on 2007-04-09.
#  Copyright (c) 2007. All rights reserved.
#
# gmailCSVreader.rb - convert a Gmail CSV export for Outlook into
# a VCard list.
# VCards are defined by RFC 2426 (http://www.ietf.org/rfc/rfc2426.txt)
# and RFB 2425 (http://www.ietf.org/rfc/rfc2425.txt).
#
require 'csv.rb'
require 'Address.rb'
require 'PersonName.rb'

header = nil;
values = [];


CSV::Reader.parse(File.open('gmail-to-outlook.csv', 'rb')) do |row|
  if(header == nil)
    header = row
  else
    if(header.length != row.length)
      puts "ERROR: header count (#{header.length}) != row count (#{row.length})"
      exit 1
    end
    
    h = {}
    
    for i in (0..header.length)
      h[header[i]] = row[i]
    end
    
    values.push h
      
  end
  
end

values.each { |row|
  puts "begin:vcard"
  puts "version:3.0"
  
  row.each_pair {|key, value|
    if value != nil and value.length > 0
      
      field = nil
      
      case key
      when "Name" then field = "fn"
      when "E-mail Address" then field = "email;type=internet,pref"
      when "Notes" then field = "note"
      when "E-mail 2" then field = "email;type=internet"
      when "E-mail 3" then field = "email;type=internet"
      when "Mobile Phone" then field = "tel;type=cell"
      when "Pager" then field = "tel;type=pager"
      when "Company" then field = "org"
      when "Job Title" then field = "title"
      when "Home Phone" then field = "tel;type=home"
      when "Home Phone 2" then field = "tel;type=home"
      when "Home Fax" then field = "tel;type=home,fax"
      when "Home Address" then field = "adr;type=home"
      when "Business Phone" then field = "tel;type=work"
      when "Business Phone 2" then field = "tel;type=work"
      when "Business Fax" then field = "tel;type=work,fax"
      when "Business Address" then field = "adr;type=work"
      when "Other Phone" then field = "tel;"
      when "Other Fax" then field = "tel;type=fax"
      when "Other Address" then field = "adr;"
      end
      
      if field
        # If it is an address, break it into components.
        if field =~ /^adr/
          a = Address.new value
          value = ";#{a.extended};#{a.street};#{a.locality};#{a.region};#{a.postalCode};#{a.country}"
        elsif field =~ /^org/
          value += ";"
        elsif field =~ /^fn/
          # If this fn value doesn't just look like an e-mail address, then try to break it into
          # component parts and then write an N record
          if !(value =~ /@/)
            pn = PersonName.parseName value
            puts "n:#{pn.family};#{pn.given};#{pn.additionals.join(',')};#{pn.prefixes.join(',')};#{pn.suffixes.join(',')}"
          end
        end

        # Escape commas.
        v = value.gsub(/,/, '\,')
        
        # Escape newlines.
        v.gsub!(/(\r\n)|(\n)|(\r)/, '\n')
        
        puts "#{field}:#{v}"
      end
    end
  }
  
  puts "end:vcard"
  puts
}


