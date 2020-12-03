from django.db import models
from django.contrib.auth.models import User

class Note(models.Model):
    user = models.ForeignKey(User)
    text = models.TextField()
    top = models.FloatField()
    left = models.FloatField()
    width = models.FloatField()
    height = models.FloatField()
