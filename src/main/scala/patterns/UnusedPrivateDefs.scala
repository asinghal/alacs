/*
 * UnusedPrivateDefs
 * Created on : Fri Sep 30 00:58:44 IST 2011
 * Author : Aishwarya Singhal
 */
package com.github.alacs.patterns;

import scala.tools.nsc.Global
import scala.reflect.generic._

import com.github.alacs.{Bug, BugInfo, BugPattern}

/**
 * TODO
 *
 * @author Aishwarya Singhal
 */
class UnusedPrivateDefs(global: Global) extends PatternDetector(global)  {
  import global._

  override val pattern = BugPattern(4, BugInfo("unused private def"))

  override def analyzeTree(tree: GTree) = {
    val bug = Bug(pattern, tree.pos)

    tree match {
      case tree@ClassDef(_,_,_,_) => analyzeBlock(tree)
      case tree@ModuleDef(_,_,_) => analyzeBlock(tree)
      case tree@DefDef(_,_,_,_,_,_) => analyzeBlock(tree, true)

      case _ => None
    }
  }

  private def analyzeBlock(tree: Tree, insideMethod: Boolean = false) = {
    var bug: Option[Bug] = None

    var privateMethods = Map[String, Bug]()

    // first pass: collect all private methods' names
    if (!tree.children.isEmpty) {
      // *** NOTE: THIS NEEDS TO BE FIXED. CAN NOT ASSUME THAT WE WILL HAVE NODES ***
      val treeRoot = if (!insideMethod) tree.children(0) else {
        var b = tree
        tree.children.foreach {
          subtree => subtree match {
            case Block(_,_) => b = subtree
            case _ => 
          }
        }

        b
      }

      treeRoot.children.foreach {
        subtree => subtree match {
          case DefDef(m: Modifiers, n, _, vparamss, _, _) => if (m.isPrivate || insideMethod) {
            // method info should be stored with param types, but how would we infer the type at call in case of implicits?!
            // println(vparamss.map{ _.map { _.toString.split(" ")(2) } }.mkString)
            privateMethods = privateMethods.updated(n.toString, Bug(pattern, subtree.pos));
          }
          case _ => // ignore
        }
      }

      def lookForReferences(subtree: Tree) {
        subtree match {
          case Ident(name) => privateMethods -= name.toString
          case _ => subtree.children.foreach { lookForReferences }
        }
      }

      // second pass: look for references
      treeRoot.children.foreach { lookForReferences }
    }

    privateMethods.keys.foreach { key => bug = report(privateMethods(key)) }

    bug
  }
}