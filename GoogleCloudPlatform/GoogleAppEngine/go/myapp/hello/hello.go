package hello

import (
    "appengine"
    "appengine/datastore"
    "appengine/user"
    "http"
    "template"
    "time"
)

type Greeting struct {
    Author  string
    Content string
    Date    datastore.Time
}

func init() {
    http.HandleFunc("/", root)
    http.HandleFunc("/sign", sign)
}

func root(w http.ResponseWriter, r *http.Request) {
    c := appengine.NewContext(r)
    q := datastore.NewQuery("Greeting").Order("-Date").Limit(10)
    greetings := make([]Greeting, 0, 10)
    if _, err := q.GetAll(c, &greetings); err != nil {
        http.Error(w, err.String(), http.StatusInternalServerError)
        return
    }
    if err := guestbookTemplate.Execute(w, greetings); err != nil {
        http.Error(w, err.String(), http.StatusInternalServerError)
    }
}

var guestbookTemplate = template.MustParse(guestbookTemplateHTML, nil)

const guestbookTemplateHTML = `
<html>
  <body>
    {.repeated section @}
      {.section Author}
        <p><b>{@|html}</b> wrote:</p>
      {.or}
        <p>An anonymous person wrote:</p>
      {.end}
      <pre>{Content|html}</pre>
    {.end}
    <form action="/sign" method="post">
      <div><textarea name="content" rows="3" cols="60"></textarea></div>
      <div><input type="submit" value="Sign Guestbook"></div>
    </form>
  </body>
</html>
`

func sign(w http.ResponseWriter, r *http.Request) {
    c := appengine.NewContext(r)
    g := Greeting{
        Content: r.FormValue("content"),
        Date:    datastore.SecondsToTime(time.Seconds()),
    }
    if u := user.Current(c); u != nil {
        g.Author = u.String()
    }
    _, err := datastore.Put(c, datastore.NewIncompleteKey("Greeting"), &g)
    if err != nil {
        http.Error(w, err.String(), http.StatusInternalServerError)
        return
    }
    http.Redirect(w, r, "/", http.StatusFound)
}