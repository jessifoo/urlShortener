
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import urlPlain._

class urlPlainTests extends FlatSpec with ShouldMatchers with BeforeAndAfterEach {
    override def beforeEach() {
      //Fixtures.deleteDatabase()
  } 
  
  "hashUrl method" should "convert a url to a hash" in {
      val url = "http://google.com"
      val url2 = "http://GOOGLE.com"
      val hashUrl = urlPlain.hashUrl(url)
      val hashUrl2 = urlPlain.hashUrl(url2)
      
      hashUrl should not be (None)
      hashUrl2 should not be (None)
      hashUrl should fullyMatch regex ("""[a-zA-Z0-9]+""")
      hashUrl2 should fullyMatch regex ("""[a-zA-Z0-9]+""")
  }
  it should "throw invalidUrlException if an invalid url is submitted" in {
      val badurl = ""
      val badurl2 = "www.heyyou"
      evaluating { urlPlain.hashUrl(badurl) } should produce [invalidUrlException]
      evaluating { urlPlain.hashUrl(badurl2) } should produce [invalidUrlException]
  }
  
  "urlFromHash method" should "convert a hash to a url" in {
      val url = "http://google.com"
      val url2 = "http://GOOGLE.com"
      val hashUrl = urlPlain.hashUrl(url)
      val hashUrl2 = urlPlain.hashUrl(url2)
      val urlFromHash = urlPlain.urlFromHash(hashUrl)
      val urlFromHash2 = urlPlain.urlFromHash(hashUrl2)
      println(urlFromHash)
      println(urlFromHash2)
      
      urlFromHash should not be (None)
      urlFromHash2 should not be (None)
      urlFromHash should fullyMatch regex ("""\(?\bhttp://[-A-Za-z0-9+&@#/%?=~_()|!:,.;]*[-A-Za-z0-9+&@#/%=~_()|]""")
      urlFromHash2 should fullyMatch regex ("""\(?\bhttp://[-A-Za-z0-9+&@#/%?=~_()|!:,.;]*[-A-Za-z0-9+&@#/%=~_()|]""")
        
  }
  it should "throw invalidHashException if an invalid hash is submitted" in {
      val badhash = ""
      evaluating { urlPlain.urlFromHash(badhash) } should produce [invalidHashException]
  }
  
  "statsForHash method" should "give stats for a hash" in {
      val url = "http://google.com"
      val url2 = "http://GOOGLE.com"
      val hashUrl = urlPlain.hashUrl(url)
      val hashUrl2 = urlPlain.hashUrl(url2)
      val stats = urlPlain.statsForHash(hashUrl)
      val stats2 = urlPlain.statsForHash(hashUrl2)
      
      stats should not be (None)
      stats2 should not be (None)
      stats should contain key ("Number of times this url has been clicked: ")
      stats2 should contain key ("Number of times this url has been clicked: ")
      stats should contain value (1)
      stats2 should contain value(1)
  }
  it should "throw invalidHashException if an invalid hash is submitted" in {
      val badhash = ""
      evaluating { urlPlain.urlFromHash(badhash) } should produce [invalidHashException]
  }
  
  "attachPrefix method" should "attach appropriate prefix when necessary" in {
      val url = "google.com"
      val url2 = "www.google.com"
      val url3 = "http://www.google.com"
      
      val prefix = urlPlain.attachPrefix(url)
      val prefix2 = urlPlain.attachPrefix(url2)
      val prefix3 = urlPlain.attachPrefix(url3)
      
      prefix should not be (None)
      prefix2 should not be (None)
      prefix3 should not be (None)
      prefix should equal ("http://www.google.com")
      prefix2 should equal ("http://www.google.com")
      prefix3 should equal ("http://www.google.com")
  }
  
  "validUrl method" should "determine if a url is valid or not" in {
      val url = "google.com"
      val url2 = "www.google.com"
      val url3 = "http://www.google.com"
      val url4 = "www.abd"
      val url5 = ""
      
      val one = urlPlain.validUrl(url)
      val two = urlPlain.validUrl(url2)
      val three = urlPlain.validUrl(url3)
      val four = urlPlain.validUrl(url4)
      val five = urlPlain.validUrl(url5)
      
      one should be (false)
      two should be (false)
      three should be (true)
      four should be (false)
      five should be (false)
    }
    
    "create method" should "return hash" in {
      val url = "http://google.com"
        
      val hash = urlPlain.create(url)
      val urlInMap = urlPlain.hashUrlMap(hash)
            
      hash should fullyMatch regex ("""[a-zA-Z0-9]+""")
      urlInMap should fullyMatch regex ("""\(?\bhttp://[-A-Za-z0-9+&@#/%?=~_()|!:,.;]*[-A-Za-z0-9+&@#/%=~_()|]""")
    }
  
    "convertTo62 method" should "return right hash" in {
      val hash = urlPlain.convertTo62(12345)
      val hashBig = urlPlain.convertTo62(2147483647)
      val hashSmall = urlPlain.convertTo62(0)
     
      hash should equal("3D7")
      hashBig should equal("2LKcb1")
      hashSmall should equal("0")
        
    }
      
    "remainder method" should "return right value" in {
      val rem = urlPlain.remainder(0,1)
      val rem2 = urlPlain.remainder(560,1)
      val rem3 = urlPlain.remainder(2147483647,1)
      
      rem should equal(0.0)
      rem2 should equal(2.0)
      rem3 should equal(1.0)
    }
}
