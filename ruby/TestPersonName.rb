#!/usr/bin/env ruby
#
#  Created by Douglas Richardson on 2007-04-12.
#  Copyright (c) 2007. All rights reserved.

require 'PersonName.rb'
require 'test/unit'

class TC_PersonNameTest < Test::Unit::TestCase
  # def setup
  # end
  
  # def teardown
  # end
  
  def testWithArrayArguments
    p = PersonName.new ["Mr.", "Dr."], "Douglas", ["Ryan", "Muchacho", "Rodrigo"], "Richardson", ["Esq.", "PhD"]
    assert_equal(2, p.prefixes.length)
    assert_equal("Mr.", p.prefixes[0])
    assert_equal("Dr.", p.prefixes[1])
    assert_equal("Douglas", p.given)
    assert_equal(3, p.additionals.length)
    assert_equal("Ryan", p.additionals[0])
    assert_equal("Muchacho", p.additionals[1])
    assert_equal("Rodrigo", p.additionals[2])
    assert_equal("Richardson", p.family)
    assert_equal(2, p.suffixes.length)
    assert_equal("Esq.", p.suffixes[0])
    assert_equal("PhD", p.suffixes[1])
  end
  
  def testWithStringArguments
    p = PersonName.new "Mr.", "Douglas", "Ryan", "Richardson", "Esq."
    assert_equal(1, p.prefixes.length)
    assert_equal("Mr.", p.prefixes[0])
    assert_equal("Douglas", p.given)
    assert_equal(1, p.additionals.length)
    assert_equal("Ryan", p.additionals[0])
    assert_equal("Richardson", p.family)
    assert_equal(1, p.suffixes.length)
    assert_equal("Esq.", p.suffixes[0])
  end
  
  def testWithNilArguments
    p = PersonName.new nil, nil, nil, nil, nil
    assert_equal(0, p.prefixes.length)
    assert_nil(p.given)
    assert_equal(0, p.additionals.length)
    assert_nil(p.family)
    assert_equal(0, p.suffixes.length)
  end
  
  def testNormalCompleteName
    p = PersonName.parseName "Mr. Douglas Ryan Richardson Jr."
    assert_equal(1, p.prefixes.length)
    assert_equal("Mr.", p.prefixes[0])
    assert_equal("Douglas", p.given)
    assert_equal(1, p.additionals.length)
    assert_equal("Ryan", p.additionals[0])
    assert_equal("Richardson", p.family)
    assert_equal(1, p.suffixes.length)
    assert_equal("Jr.", p.suffixes[0])
  end
  
  def testParseNormalCompleteNameUpperCased
    p = PersonName.parseName "MR. DOUGLAS RYAN RICHARDSON JR."
    assert_equal(1, p.prefixes.length)
    assert_equal("MR.", p.prefixes[0])
    assert_equal("DOUGLAS", p.given)
    assert_equal(1, p.additionals.length)
    assert_equal("RYAN", p.additionals[0])
    assert_equal("RICHARDSON", p.family)
    assert_equal(1, p.suffixes.length)
    assert_equal("JR.", p.suffixes[0])
  end
  
  def testParsePrefixAndFamily
    p = PersonName.parseName "Mr. Richardson"
    assert_equal(1, p.prefixes.length)
    assert_equal("Mr.", p.prefixes[0])
    assert_nil(p.given)
    assert_equal(0, p.additionals.length)
    assert_equal("Richardson", p.family)
    assert_equal(0, p.suffixes.length)
  end
  
  def testParseFamilyFirst
    p = PersonName.parseName "Richardson, Mr. Douglas Ryan Jr."
    assert_equal(1, p.prefixes.length)
    assert_equal("Mr.", p.prefixes[0])
    assert_equal("Douglas", p.given)
    assert_equal(1, p.additionals.length)
    assert_equal("Ryan", p.additionals[0])
    assert_equal("Richardson", p.family)
    assert_equal(1, p.suffixes.length)
    assert_equal("Jr.", p.suffixes[0])
  end
  
  def testParseSuffixes
    p = PersonName.parseName "Dr. Douglas Ryan Richardson Jr., Esq., Esquire, M.D., MD, PhD, Ph.D., CPA, C.P.A."
    assert_equal(1, p.prefixes.length)
    assert_equal("Dr.", p.prefixes[0])
    assert_equal("Douglas", p.given)
    assert_equal(1, p.additionals.length)
    assert_equal("Ryan", p.additionals[0])
    assert_equal("Richardson", p.family)
    assert_equal(9, p.suffixes.length)
    assert_equal("Jr.", p.suffixes[0])
    assert_equal("Esq.", p.suffixes[1])
    assert_equal("Esquire", p.suffixes[2])
    assert_equal("M.D.", p.suffixes[3])
    assert_equal("MD", p.suffixes[4])
    assert_equal("PhD", p.suffixes[5])
    assert_equal("Ph.D.", p.suffixes[6])
    assert_equal("CPA", p.suffixes[7])
    assert_equal("C.P.A.", p.suffixes[8])
  end
  
  def testParseFirstNameOnly
    p = PersonName.parseName "Douglas"
    assert_equal(0, p.prefixes.length)
    assert_equal("Douglas", p.given)
    assert_equal(0, p.additionals.length)
    assert_nil(p.family)
    assert_equal(0, p.suffixes.length)
  end
  
  def testParseNameWithAbbreviations
    p = PersonName.parseName "Mr H. R. Hughes"
    assert_equal(1, p.prefixes.length)
    assert_equal("Mr", p.prefixes[0])
    assert_equal("H.", p.given)
    assert_equal(1, p.additionals.length)
    assert_equal("R.", p .additionals[0])
    assert_equal("Hughes", p.family)
    assert_equal(0, p.suffixes.length)
  end
  
  def testParseMrsNameWithAbbreviations
    p = PersonName.parseName "Ms. Julian H. Kilroy Q. Hartford-Brandon Esq., PhD"
    assert_equal(1, p.prefixes.length)
    assert_equal("Ms.", p.prefixes[0])
    assert_equal("Julian", p.given)
    assert_equal(3, p.additionals.length)
    assert_equal("H.", p .additionals[0])
    assert_equal("Kilroy", p .additionals[1])
    assert_equal("Q.", p .additionals[2])
    assert_equal("Hartford-Brandon", p.family)
    assert_equal(2, p.suffixes.length)
    assert_equal("Esq.", p.suffixes[0])
    assert_equal("PhD", p.suffixes[1])
  end
  
end
