#!/usr/bin/env ruby
#
#  Created by Douglas Richardson on 2007-04-12.
#  Copyright (c) 2007. All rights reserved.

class PersonName  
  def initialize(prefixes, given, additionalNames, family, suffixes)
    if prefixes.class != Array
      if prefixes == nil
        prefixes = []
      else
        prefixes = [prefixes]
      end
    end
    @prefixes = prefixes
    
    @given = given
    
    if additionalNames.class != Array
      if additionalNames == nil
        additionalNames = []
      else
        additionalNames = [additionalNames]
      end
    end
    @additionalNames = additionalNames
    
    @family = family
    
    if suffixes.class != Array
      if suffixes == nil
        suffixes = []
      else
        suffixes = [suffixes]
      end
    end
    @suffixes = suffixes    
  end
  
  def prefixes
    @prefixes
  end
  
  def given
    @given
  end
    
  def additionals
    @additionalNames
  end
  
  def family
    @family
  end
  
  def suffixes
    @suffixes
  end
  
  def PersonName.parseName(name)
    prefixes = []
    suffixes = []
    partsLeft = []
    parts = name.split(/\s*,\s*|\s+/)
    
    isFamilyNameFirst = name =~ /^\s*(\S+)\s*,.*/
    
    parts.each { |p|
      if p =~ /((Mr|Mrs|Miss|Ms|Dr)\.?)/i
        prefixes.push p
      elsif p =~ /^\S\.$/
        # Match name abbreviations like H. and R.
        partsLeft.push p
      elsif p =~ /(\S+\.\S*)|DD|MD|JD|CPA|PhD|Esq|Esquire/i
        suffixes.push p
      else
        partsLeft.push p
      end
    }
    
    if partsLeft.length == 1
      if prefixes.length >= 1
        family = partsLeft.pop
      else
        given = partsLeft.pop
      end
    elsif isFamilyNameFirst
      family = partsLeft.first
      partsLeft.delete_at 0
      given = partsLeft.first
      partsLeft.delete_at 0
    else
      given = partsLeft.first
      partsLeft.delete_at 0
      family = partsLeft.pop
    end
    
    additionals = partsLeft
    
    PersonName.new prefixes, given, additionals, family, suffixes
  end
  
end
