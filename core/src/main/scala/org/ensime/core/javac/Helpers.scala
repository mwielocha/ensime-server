// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import akka.event.slf4j.SLF4JLogging
import com.sun.source.tree._
import com.sun.source.util.TreePath
import javax.lang.model.`type`.{ DeclaredType, PrimitiveType, TypeKind, TypeMirror }
import javax.lang.model.element._
import scala.collection.JavaConversions._

import org.ensime.api.deprecating
import org.ensime.core.{ DocFqn, DocSig }
import org.ensime.indexer._

trait Helpers extends UnsafeHelpers with SLF4JLogging {

  def typeMirror(c: Compilation, t: Tree): Option[TypeMirror] = {
    Option(c.trees.getTypeMirror(c.trees.getPath(c.compilationUnit, t)))
  }

  def typeElement(c: Compilation, t: Tree): Option[Element] = {
    typeMirror(c, t).map(c.types.asElement)
  }

  def element(c: Compilation, path: TreePath): Option[Element] = {
    Option(c.trees.getElement(path))
      .orElse(unsafeGetElement(path.getLeaf))
      .orElse(Option(c.trees.getTypeMirror(path))
        .flatMap(t => Option(c.types.asElement(t))))
  }

  def fqn(c: Compilation, el: Element): Option[FullyQualifiedName] = el match {
    case e: ExecutableElement =>

      descriptor(c, e).map {
        MethodName(
          ClassName.fromFqn(e.getEnclosingElement.toString),
          e.getSimpleName.toString, _
        )
      }

    case e => fqn(c, e.asType())
  }

  private def descriptor(c: Compilation, e: ExecutableElement): Option[Descriptor] = {
    import scala.collection.breakOut
    fqn(c, e.getReturnType).map { returnType =>
      val params: List[DescriptorType] = e.getParameters.flatMap(p => fqn(c, p.asType()))(breakOut)
      Descriptor(params, returnType)
    }
  }

  def path(c: Compilation, t: Tree): Option[TreePath] = {
    Option(c.trees.getPath(c.compilationUnit, t))
  }

  def fqn(c: Compilation, t: Tree): Option[FullyQualifiedName] = {
    path(c, t).flatMap(fqn(c, _))
  }

  def fqn(c: Compilation, p: TreePath): Option[FullyQualifiedName] = {
    element(c, p).flatMap(fqn(c, _))
  }

  def fqn(c: Compilation, tm: TypeMirror): Option[ClassName] = {
    // "Using instanceof is not necessarily a reliable idiom for
    // determining the effective class of an object in this modeling
    // hierarchy since an implementation may choose to have a single
    // object implement multiple TypeMirror subinterfaces." --
    // TypeMirror docs
    tm match {
      case tm: DeclaredType if tm.getKind == TypeKind.DECLARED => {
        tm.asElement match {
          case te: TypeElement => Some(ClassName.fromFqn(te.getQualifiedName.toString))
          case _ => {
            None
          }
        }
      }
      case tm: PrimitiveType if tm.getKind.isPrimitive => Some(ClassName(PackageName(Nil), tm.toString))
      case _ => None
    }
  }

  def toDocSign(fqn: FullyQualifiedName): DocSig = fqn match {
    case p: PackageName => DocSig(DocFqn(p.parent.fqnString, p.path.last), None)
    case c: ClassName => DocSig(DocFqn(c.pack.fqnString, c.name), None)
    case m: MethodName => DocSig(DocFqn(m.owner.fqnString, m.name), Some(m.name))
    case f: FieldName => DocSig(DocFqn(f.owner.fqnString, f.name), Some(f.name))
  }
}
