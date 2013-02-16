#!/usr/bin/env ruby
#
#  Created by Douglas Richardson on 2007-04-11.
#  Copyright (c) 2007. All rights reserved.

class Address
  private

  LocalityRegionPostalCode = /([^,]+)[ \t]*,[ \t]*([^,]+)[ \t,]+(\S*\d\S*)/
  LocalityRegion = /([^,]+)[ \t]*,[ \t]*([^,]+)/

  ExtendedKey = "Extended"
  StreetKey = "Street"
  LocalityKey = "Locality"
  RegionKey = "Region"
  PostalCodeKey = "PostalCode"
  CountryKey = "Country"

  def extractLocalityParts(address)

    count = (address =~ LocalityRegionPostalCode)

    parts = nil

    if count
      parts = { LocalityKey => $1, RegionKey => $2, PostalCodeKey => $3 }
    else
      count = (address =~ LocalityRegion)
      if count
        parts = { LocalityKey => $1, RegionKey => $2 }
      end
    end

    parts

  end

  def extractAddressParts(address)
    addressLines = address.split(/\n/)
    addressLines.each { |line| line.chomp! }

    parts = nil

    case addressLines.length
    when 1 then
      parts = extractLocalityParts addressLines[0]
      if !parts
        parts = { StreetKey => addressLines[0] }
      end

    when 2 then
      if (parts = extractLocalityParts addressLines[1]) != nil
        parts[StreetKey] = addressLines[0]
      elsif (parts = extractLocalityParts addressLines[0]) != nil
        parts[CountryKey] = addressLines[1]
      else
        parts = { ExtendedKey => addressLines[0], StreetKey => addressLines[1] }
      end

    when 3 then
      if (parts = extractLocalityParts addressLines[2]) != nil
        parts[ExtendedKey] = addressLines[0]
        parts[StreetKey] = addressLines[1]
      elsif (parts = extractLocalityParts addressLines[1]) != nil
        parts[StreetKey] = addressLines[0]
        parts[CountryKey] = addressLines[2]
      else
        parts = {}
        parts[ExtendedKey] = addressLines[0]
        parts[StreetKey] = addressLines[1]
        parts[CountryKey] = addressLines[2]
      end

    when 4 then
      if (parts = extractLocalityParts addressLines[2]) != nil
        parts[ExtendedKey] = addressLines[0]
        parts[StreetKey] = addressLines[1]
        parts[CountryKey] = addressLines[3]
      else
        parts = nil
      end
    end

    parts

  end
  
  public
  
  def initialize address
    tmp = extractAddressParts(address)
    if tmp
      @parts = tmp
    else
      @parts = {}
    end
  end
  
  def extended
    @parts[ExtendedKey]
  end
  
  def street
    @parts[StreetKey]
  end
  
  def locality
    @parts[LocalityKey]
  end
  
  def region
    @parts[RegionKey]
  end
  
  def postalCode
    @parts[PostalCodeKey]
  end
  
  def country
    @parts[CountryKey]
  end    
  
end


