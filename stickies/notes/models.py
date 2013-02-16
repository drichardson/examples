from google.appengine.ext import db

class Note(db.Model):
    user = db.UserProperty()
    text = db.StringProperty(multiline=True)
    date = db.DateTimeProperty(auto_now_add=True)
    left = db.IntegerProperty()
    top = db.IntegerProperty()
    width = db.IntegerProperty()
    height = db.IntegerProperty()