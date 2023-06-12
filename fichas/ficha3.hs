data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String deriving Show
              
type Nome = String
type Agenda = [(Nome, [Contacto])]

--3 a) dado um nome, um email e uma agenda, acrescenta essa informacao a agenda
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail  nome email [] = [(nome, [email])] 
acrescEmail nome email ((n,cs):t) | nome == n = 

