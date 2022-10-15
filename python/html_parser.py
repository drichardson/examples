from html.parser import HTMLParser

class MyHTMLParser(HTMLParser):
    def __init__(self):
        self.last_data = None
        super().__init__()

    def handle_starttag(self, tag, attrs):
        print("start: ", tag)
        print("  attrs: ", attrs)

    def handle_endtag(self, tag):
        print("end: ", tag)

    def handle_data(self, data):
        print("data: ", data)
        self.last_data = data

parser = MyHTMLParser()
parser.feed('<html><head><title>Test</title></head>'
            '<body><h1 id="myattr">Parse me!</h1></body></html>')

print("last data is: ", parser.last_data)


