package soot.util;

/*-
 * #%L
 * Soot - a J*va Optimization Framework
 * %%
 * Copyright (C) 1997 - 1999 Raja Vallee-Rai
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

import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Provides an implementation of the Set object using java.util.Array
 */

public class ArraySet<E> extends AbstractSet<E> {
  private static final int DEFAULT_SIZE = 8;

  private int numElements;
  private int maxElements;
  private Object[] elements;

  public ArraySet(int size) {
    maxElements = size;
    elements = new Object[size];
    numElements = 0;
  }

  public ArraySet() {
    this(DEFAULT_SIZE);
  }

  /**
   * Create a set which contains the given elements.
   */

  public ArraySet(E[] elements) {
    this();

    for (E element : elements) {
      add(element);
    }
  }

  @Override
  final public void clear() {
    numElements = 0;
  }

  @Override
  final public boolean contains(Object obj) {
    for (int i = 0; i < numElements; i++) {
      if (elements[i].equals(obj)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Add an element without checking whether it is already in the set. It is up to the caller to guarantee that it isn't.
   */
  final public boolean addElement(E e) {
    if (e == null) {
      throw new RuntimeException("oops");
    }
    // Expand array if necessary
    if (numElements == maxElements) {
      doubleCapacity();
    }

    // Add element
    elements[numElements++] = e;
    return true;
  }

  @Override
  final public boolean add(E e) {
    if (e == null) {
      throw new RuntimeException("oops");
    }
    if (contains(e)) {
      return false;
    } else {
      // Expand array if necessary
      if (numElements == maxElements) {
        doubleCapacity();
      }

      // Add element
      elements[numElements++] = e;
      return true;
    }
  }

  @Override
  @SuppressWarnings("unchecked")
  final public boolean addAll(Collection<? extends E> s) {
    boolean ret = false;
    if (!(s instanceof ArraySet)) {
      return super.addAll(s);
    }
    ArraySet<?> as = (ArraySet<?>) s;
    int asSize = as.size();
    Object[] asElements = as.elements;
    for (int i = 0; i < asSize; i++) {
      ret = add((E) asElements[i]) | ret;
    }
    return ret;
  }

  @Override
  final public int size() {
    return numElements;
  }

  @Override
  final public Iterator<E> iterator() {
    return new ArrayIterator<E>();
  }

  private class ArrayIterator<V> implements Iterator<V> {
    int nextIndex;

    ArrayIterator() {
      nextIndex = 0;
    }

    @Override
    final public boolean hasNext() {
      return nextIndex < numElements;
    }

    @Override
    @SuppressWarnings("unchecked")
    final public V next() throws NoSuchElementException {
      if (!(nextIndex < numElements)) {
        throw new NoSuchElementException();
      }

      return (V) elements[nextIndex++];
    }

    @Override
    final public void remove() throws NoSuchElementException {
      if (nextIndex == 0) {
        throw new NoSuchElementException();
      } else {
        removeElementAt(nextIndex - 1);
        nextIndex = nextIndex - 1;
      }
    }
  }

  final private void removeElementAt(int index) {
    // Handle simple case
    if (index == numElements - 1) {
      numElements--;
      return;
    }

    // Else, shift over elements
    System.arraycopy(elements, index + 1, elements, index, numElements - (index + 1));
    numElements--;
  }

  final private void doubleCapacity() {
    // plus one to ensure that we have at least one element
    int newSize = maxElements * 2 + 1;

    Object[] newElements = new Object[newSize];

    System.arraycopy(elements, 0, newElements, 0, numElements);
    elements = newElements;
    maxElements = newSize;
  }

  @Override
  final public Object[] toArray() {
    Object[] array = new Object[numElements];

    System.arraycopy(elements, 0, array, 0, numElements);
    return array;
  }

  @SuppressWarnings("unchecked")
  @Override
  final public <T> T[] toArray(T[] array) {
    if (array.length < numElements) {
      return (T[]) Arrays.copyOf(elements, numElements, array.getClass());
    } else {
      System.arraycopy(elements, 0, array, 0, numElements);
      if (array.length > numElements) {
        array[numElements] = null;
      }
      return array;
    }
  }

  final public Object[] getUnderlyingArray() {
    return elements;
  }

  class Array {
    private final int DEFAULT_SIZE = 8;

    private int numElements;
    private int maxElements;
    private Object[] elements;

    final public void clear() {
      numElements = 0;
    }

    public Array() {
      elements = new Object[DEFAULT_SIZE];
      maxElements = DEFAULT_SIZE;
      numElements = 0;
    }

    final private void doubleCapacity() {
      int newSize = maxElements * 2;

      Object[] newElements = new Object[newSize];

      System.arraycopy(elements, 0, newElements, 0, numElements);
      elements = newElements;
      maxElements = newSize;
    }

    final public void addElement(Object e) {
      // Expand array if necessary
      if (numElements == maxElements) {
        doubleCapacity();
      }

      // Add element
      elements[numElements++] = e;
    }

    final public void insertElementAt(Object e, int index) {
      // Expaxpand array if necessary
      if (numElements == maxElements) {
        doubleCapacity();
      }

      // Handle simple case
      if (index == numElements) {
        elements[numElements++] = e;
        return;
      }

      // Shift things over
      System.arraycopy(elements, index, elements, index + 1, numElements - index);
      elements[index] = e;
      numElements++;
    }

    final public boolean contains(Object e) {
      for (int i = 0; i < numElements; i++) {
        if (elements[i].equals(e)) {
          return true;
        }
      }

      return false;
    }

    final public int size() {
      return numElements;
    }

    final public Object elementAt(int index) {
      return elements[index];
    }

    final public void removeElementAt(int index) {
      // Handle simple case
      if (index == numElements - 1) {
        numElements--;
        return;
      }

      // Else, shift over elements
      System.arraycopy(elements, index + 1, elements, index, numElements - (index + 1));
      numElements--;
    }
  }
}
