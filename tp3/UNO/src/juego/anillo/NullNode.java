package juego.anillo;

import juego.jugador.Jugador;

public class NullNode extends AbstractNode {

    @Override
    public AbstractNode add(AbstractNode node, AbstractNode current) {
        // Aquí podrías retornar un RealNode, o 'node', según tu lógica.
        // Por ahora, simplemente retorna el nodo que llega.
        return node;
    }

    @Override
    public AbstractNode remove(AbstractNode node) {
        // Lanza excepción, ya que no tiene sentido remover desde un NullNode.
        throw new UnsupportedOperationException("No se puede eliminar desde un NullNode.");
    }

    @Override
    public AbstractNode next(AbstractNode node) {
        // Lanza excepción, ya que no hay un "siguiente" en NullNode.
        throw new UnsupportedOperationException("No hay siguiente nodo en un NullNode.");
    }

    @Override
    public Jugador current(AbstractNode node) {
        // Lanza excepción, ya que no existe un nodo actual en NullNode.
        throw new UnsupportedOperationException("No hay nodo actual en un NullNode.");
    }
    @Override
    public AbstractNode previous(AbstractNode node) {
        // Lanza excepción, ya que no hay un "anterior" en NullNode.
        throw new UnsupportedOperationException("No hay nodo anterior en un NullNode.");
    }
}

