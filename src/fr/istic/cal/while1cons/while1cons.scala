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
  // TODO TP4
  def while1ConsExprV(expression: Expression): (List[Command], Variable) = {
    expression match {
      case Nl => {val res: Variable = NewVar.make(); (List(Set(res, Nl)), res)}
      case VarExp(x) => {(Nil, Var(x))}
      case Cst(x) => {val res: Variable = NewVar.make(); (List(Set(res, Cst(x))), res)}
    }
    
  }

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return une paire constituée d'une liste d'affectations et une expression simple
   * qui, combinées, ont le même effet que l'expression initiale
   */
  // TODO TP4
  def while1ConsExprSE(expression: Expression): (List[Command], Expression) = ???

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */
  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @return une liste de commandes ayant un seul constructeur par expression
   * et ayant le même effet que la commande initiale
   */
  // TODO TP4
  def while1ConsCommand(command: Command): List[Command] = ???

  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @return une liste de commandes ayant un seul constructeur par expression
   * et ayant le même effet que les commandes initiales
   */
  // TODO TP4
  def while1ConsCommands(commands: List[Command]): List[Command] = ???

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @return un AST décrivant un programme du langage WHILE 
   * de même sémantique que le programme initial mais ne contenant que des expressions simples
   */
  // TODO TP4
  def while1ConsProgr(program: Program): Program = ???

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