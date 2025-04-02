package anillo;

import java.util.NoSuchElementException;

public class Ring {
    private Object cargo;
    private Ring next;
    private boolean empty;

    // Constructor: crea un anillo vacío.
    public Ring() {
        empty = true;
        next = this;
    }

    // Devuelve el cargo del nodo actual o lanza una excepción si el anillo está vacío.
    public Object current() {
        if (empty)
            throw new NoSuchElementException("El anillo está vacío");
        return cargo;
    }

    // Devuelve el siguiente nodo del anillo o lanza una excepción si el anillo está vacío.
    public Ring next() {
        if (empty)
            throw new NoSuchElementException("El anillo está vacío");
        return next;
    }

    // Inserta un nuevo cargo en el anillo de forma que el nuevo cargo se convierte en el actual.
    public Ring add(Object cargo) {
        if (cargo == null)
            throw new NullPointerException("El cargo no puede ser nulo");
        if (empty) {
            this.cargo = cargo;
            empty = false;
            next = this;
            return this;
        } else {
            Ring newNode = new Ring();
            newNode.cargo = cargo;
            newNode.empty = false;
            newNode.next = this.next;
            this.next = newNode;
            // Intercambio de cargos para que el nuevo cargo pase a ser el actual
            Object temp = this.cargo;
            this.cargo = newNode.cargo;
            newNode.cargo = temp;
            return this;
        }
    } // Se agrega antes que el current ("a izquierda") y devuelve el "nuevo"

    // Elimina el nodo actual del anillo.
// Si el anillo tiene un solo elemento, se marca como vacío.
    public Ring remove() {
        if (empty)
            throw new NoSuchElementException("No se puede eliminar de un anillo vacío");
        if (this.next == this) {
            // Sólo un elemento: se vacía el anillo.
            this.cargo = null;
            empty = true;
            return this;
        } else {
            // Se elimina el nodo actual copiando el cargo y la referencia del siguiente nodo.
            this.cargo = this.next.cargo;
            this.next = this.next.next;
            return this;
        }
    } // Devuelve el nodo que era el siguiente cuando quise remover.


}
