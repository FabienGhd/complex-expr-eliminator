package fr.istic.cal.while1cons

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 * 
 * ETUDIANT 1 : GUIHARD Fabien
 * 
 * ETUDIANT 2 : HOMERY Axel
 * 
 */

import scala.util.Try

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object While1cons {

 

  /**
   * UN ELIMINATEUR D'EXPRESSIONS COMPLEXES POUR LE LANGAGE WHILE
   *
   */

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations ayant le même effet
   * que l'expression et de la variable qui contient le résultat
   */
  def while1ConsExprV(expression: Expression): (List[Command], Variable) = {
    expression match {
      
      case Nl        => { val res: Variable = NewVar.make(); (List(Set(res, Nl)), res) }

      case VarExp(x) => { (Nil, Var(x)) }

      case Cst(x)    => { val res: Variable = NewVar.make(); (List(Set(res, Cst(x))), res) }

      case Hd(e) => {
        var (list, v): (List[Command], Variable) = while1ConsExprV(e)
        (list, v) match {
          case (_, Var(x)) => {
            val y: Variable = NewVar.make();
            (list ++ List(Set(y, Hd(VarExp(x)))), y)
          }
        }
      }

      case Tl(e) => {
        var (list, v): (List[Command], Variable) = while1ConsExprV(e)
        (list, v) match {
          case (_, Var(x)) => {
            val new_var: Variable = NewVar.make();
            (list ++ List(Set(new_var, Tl(VarExp(x)))), new_var)
          }
        }
      }

      case Eq(e1, e2) => {
        var (list1, v1): (List[Command], Variable) = while1ConsExprV(e1)
        var (list2, v2): (List[Command], Variable) = while1ConsExprV(e2)
        (list1, list2, v1, v2) match {
          case (_, _, Var(x1), Var(x2)) => {
            val new_var: Variable = NewVar.make();
            val last_com: List[Command] = List(Set(new_var, Eq(VarExp(x1), VarExp(x2))));
            (list1 ++ list2 ++ last_com, new_var)
          }
        }
      }

      case Cons(e1, e2) => {
        var (list1, v1): (List[Command], Variable) = while1ConsExprV(e1)
        var (list2, v2): (List[Command], Variable) = while1ConsExprV(e2)
        (list1, list2, v1, v2) match {
          case (_, _, Var(x1), Var(x2)) => {
            val new_var: Variable = NewVar.make();
            val last_com: List[Command] = List(Set(new_var, Cons(VarExp(x1), VarExp(x2))));
            (list1 ++ list2 ++ last_com, new_var)
          }
        }
      }
    }
  }

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations et une expression simple
   * qui, combinées, ont le même effet que l'expression initiale
   */
  def while1ConsExprSE(expression: Expression): (List[Command], Expression) = {
    expression match {
      case Nl        => (Nil, Nl)

      case VarExp(x) => { (List(), VarExp(x)) }

      case Cst(x)    => (List(), Cst(x))

      case Hd(e) => {
        val (coms, v) = while1ConsExprV(e);
        v match {
          case Var(s) => (coms, Hd(VarExp(s)))
        }
      }

      case Tl(e) => {
        val (coms, v) = while1ConsExprV(e);
        v match {
          case Var(s) => (coms, Tl(VarExp(s)))
        }
      }

      case Eq(e1, e2) => {
        val (coms1, v1) = while1ConsExprV(e1);
        val (coms2, v2) = while1ConsExprV(e2);
        (v1, v2) match {
          case (Var(x1), Var(x2)) => (coms1 ++ coms2, Eq(VarExp(x1), VarExp(x2)))
        }
      }

      case Cons(e1, e2) => {
        val (coms1, v1) = while1ConsExprV(e1);
        val (coms2, v2) = while1ConsExprV(e2);
        (v1, v2) match {
          case (Var(x1), Var(x2)) => (coms1 ++ coms2, Cons(VarExp(x1), VarExp(x2)))
        }
      }
    }
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */
  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @return une liste de commandes ayant un seul constructeur par expression
   * et ayant le même effet que la commande initiale
   */
  def while1ConsCommand(command: Command): List[Command] = {
    command match {
      case Nop => List(Nop)
      case Set(v, e) => {
        val cons_expr = while1ConsExprSE(e) // On réduit l'expression
        cons_expr match {
          case (coms, expr) => coms ++ List(Set(v, expr)) // On renvoit la liste des commandes à effectuer + la commande finale
        }
      }
      case While(cond, body) => {
        val cons_expr = while1ConsExprV(cond)
        cons_expr._2 match {
          case Var(s) => cons_expr._1 ++ List(While(VarExp(s), while1ConsCommands(body) ++ cons_expr._1))
        }
      }
      case For(e, body) => {
        val cons_expr = while1ConsExprV(e)
        cons_expr._2 match {
          case Var(s) => cons_expr._1 ++ List(For(VarExp(s), while1ConsCommands(body)))
        }
      }
      case If(e, then_coms, else_coms) => {
        val cons_expr = while1ConsExprV(e)
        cons_expr._2 match {
          case Var(s) => cons_expr._1 ++ List(If(VarExp(s), while1ConsCommands(then_coms), while1ConsCommands(else_coms)))
        }
      }
    }
  }

  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @return une liste de commandes ayant un seul constructeur par expression
   * et ayant le même effet que les commandes initiales
   */
  def while1ConsCommands(commands: List[Command]): List[Command] = {
     commands match {
      
      case Nil                                => throw ExceptionListeVide
      case Nop :: Nil                         => List(Nop)
      case Nop :: end                         => List(Nop) ++ while1ConsCommands(end)
      case Set(v, e) :: Nil                   => while1ConsCommand(Set(v, e))
      case Set(v, e) :: end                   => while1ConsCommand(Set(v, e)) ++ while1ConsCommands(end)
      case While(e, coms) :: Nil              => while1ConsCommand(While(e, coms))
      case While(e, coms) :: end              => while1ConsCommand(While(e, coms)) ++ while1ConsCommands(end)
      case For(e, coms) :: Nil                => while1ConsCommand(For(e, coms))
      case For(e, coms) :: end                => while1ConsCommand(For(e, coms)) ++ while1ConsCommands(end)
      case If(e, then_coms, else_coms) :: Nil => while1ConsCommand(If(e, then_coms, else_coms))
      case If(e, then_coms, else_coms) :: end => while1ConsCommand(If(e, then_coms, else_coms)) ++ while1ConsCommands(end)
    }
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @return un AST décrivant un programme du langage WHILE 
   * de même sémantique que le programme initial mais ne contenant que des expressions simples
   */
  def while1ConsProgr(program: Program): Program = {
    program match {
      case Progr(in, body, out) => {
        (in, out) match {
          case (Nil, _) => throw ExceptionListeVide
          case (_, Nil) => throw ExceptionListeVide
          case _ => Progr(in, while1ConsCommands(body), out)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    
    // vous pouvez ici tester manuellement vos fonctions par des print

  }
  
  
   /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }
  
}