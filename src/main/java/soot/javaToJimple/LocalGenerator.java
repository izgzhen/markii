package soot.javaToJimple;

/*-
 * #%L
 * Soot - a J*va Optimization Framework
 * %%
 * Copyright (C) 2004 Jennifer Lhotak
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 2.1 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Lesser Public License for more details.
 *
 * You should have received a copy of the GNU General Lesser Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/lgpl-2.1.html>.
 * #L%
 */

import java.util.HashSet;
import java.util.Set;
import soot.Body;
import soot.Local;
import soot.jimple.toolkits.typing.fast.Integer127Type;
import soot.jimple.toolkits.typing.fast.Integer1Type;
import soot.jimple.toolkits.typing.fast.Integer32767Type;

public class LocalGenerator {

  protected final soot.Body body;

  public LocalGenerator(Body b) {
    body = b;
  }

  protected transient Set<String> localNames = null;

  protected boolean bodyContainsLocal(String name) {
    return localNames.contains(name);
  }

  private void initLocalNames() {
    localNames = new HashSet<String>();
    for (Local l : body.getLocals()) {
      localNames.add(l.getName());
    }
  }

  /** generates a new soot local given the type */
  public soot.Local generateLocal(soot.Type type) {

    // store local names for enhanced performance
    initLocalNames();

    String name = "v";
    if (type instanceof soot.IntType
        || type instanceof Integer1Type
        || type instanceof Integer127Type
        || type instanceof Integer32767Type) {
      while (true) {
        name = nextIntName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.ByteType) {
      while (true) {
        name = nextByteName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.ShortType) {
      while (true) {
        name = nextShortName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.BooleanType) {
      while (true) {
        name = nextBooleanName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.VoidType) {
      while (true) {
        name = nextVoidName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.CharType) {
      while (true) {
        name = nextCharName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
      type = soot.CharType.v();
    } else if (type instanceof soot.DoubleType) {
      while (true) {
        name = nextDoubleName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.FloatType) {
      while (true) {
        name = nextFloatName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.LongType) {
      while (true) {
        name = nextLongName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.RefLikeType) {
      while (true) {
        name = nextRefLikeTypeName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else if (type instanceof soot.UnknownType) {
      while (true) {
        name = nextUnknownTypeName();
        if (!bodyContainsLocal(name)) {
          break;
        }
      }
    } else {
      localNames = null;
      throw new RuntimeException(
          String.format("Unhandled Type %s of Local variable to Generate - Not Implemented", type.getClass().getName()));
    }

    localNames = null;
    return createLocal(name, type);
  }

  private int tempInt = -1;
  private int tempVoid = -1;
  private int tempBoolean = -1;
  private int tempLong = -1;
  private int tempDouble = -1;
  private int tempFloat = -1;
  private int tempRefLikeType = -1;
  private int tempByte = -1;
  private int tempShort = -1;
  private int tempChar = -1;
  private int tempUnknownType = -1;

  private String nextIntName() {
    tempInt++;
    return "$i" + tempInt;
  }

  private String nextCharName() {
    tempChar++;
    return "$c" + tempChar;
  }

  private String nextVoidName() {
    tempVoid++;
    return "$v" + tempVoid;
  }

  private String nextByteName() {
    tempByte++;
    return "$b" + tempByte;
  }

  private String nextShortName() {
    tempShort++;
    return "$s" + tempShort;
  }

  private String nextBooleanName() {
    tempBoolean++;
    return "$z" + tempBoolean;
  }

  private String nextDoubleName() {
    tempDouble++;
    return "$d" + tempDouble;
  }

  private String nextFloatName() {
    tempFloat++;
    return "$f" + tempFloat;
  }

  private String nextLongName() {
    tempLong++;
    return "$l" + tempLong;
  }

  private String nextRefLikeTypeName() {
    tempRefLikeType++;
    return "$r" + tempRefLikeType;
  }

  private String nextUnknownTypeName() {
    tempUnknownType++;
    return "$u" + tempUnknownType;
  }

  // this should be used for generated locals only
  protected soot.Local createLocal(String name, soot.Type sootType) {
    soot.Local sootLocal = soot.jimple.Jimple.v().newLocal(name, sootType);
    body.getLocals().add(sootLocal);
    return sootLocal;
  }
}
