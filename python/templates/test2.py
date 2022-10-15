
from jinja2 import Environment, PackageLoader, select_autoescape

import pdb

env = Environment(loader=PackageLoader("stuff"),
                  autoescape=select_autoescape())

env.compile_templates("compiled", ["html"], ignore_errors=False, log_function=print)
print(f"cache: {env.cache}")

pdb.set_trace()
template = env.get_template("mytemplate.html")
print(f"cache: {env.cache}")

print(template.render(the="variables", go="here"))

