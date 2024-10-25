/-
Variables and Sections
https://lean-lang.org/lean4/doc/sections.html
-/

-- Lean provides us with the variable command to make such declarations look more compact:

def compose_long (α β γ : Type) (g : β → γ) (f : α → β) (x : α) : γ :=
  g (f x)

def doTwice_long (α : Type) (h : α → α) (x : α) : α :=
  h (h x)

def doThrice_long (α : Type) (h : α → α) (x : α) : α :=
  h (h (h x))


variable (α β γ : Type)
variable (g : β → γ) (f : α → β) (h : α → α)
variable (x : α)

def compose := g (f x)
def doTwice := h (h x)
def doThrice := h (h (h x))

-- Printing them out shows that all three groups of definitions have exactly the same effect.



#print compose
#print compose_long

#print doTwice
#print doTwice_long

#print doThrice
#print doThrice_long

example : compose_long = compose := rfl
example : doTwice_long = doTwice := rfl
example : doThrice_long = doThrice := rfl

/-
The variable command instructs Lean to insert the declared variables as bound variables in definitions that refer to them. Lean is smart enough to figure out which variables are used explicitly or implicitly in a definition. We can therefore proceed as though α, β, γ, g, f, h, and x are fixed objects when we write our definitions, and let Lean abstract the definitions for us automatically.

When declared in this way, a variable stays in scope until the end of the file we are working on. Sometimes, however, it is useful to limit the scope of a variable. For that purpose, Lean provides the notion of a section:
-/
section useful
  variable (a β γ : Type)
  variable (g : β → γ) (f : α → β) (h : α → α)
  variable (x : α)

  def compose_section := g (f x)
  def doTwice_section := h (h x)
  def doThrice_section := h (h (h x))
end useful

example : compose_section = compose := rfl
example : doTwice_section = doTwice := rfl
example : doThrice_section = doThrice := rfl
