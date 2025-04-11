package anillo;


public class RealNode extends AbstractNode {

    private Object cargo;
    private RealNode next;
    private RealNode prev;

    public RealNode(Object cargo) {
        this.cargo = cargo;
        // Inicialmente, el nodo se apunta a sí mismo
        this.next = this;
        this.prev = this;
    }

    @Override
    public AbstractNode add(AbstractNode newNode, AbstractNode current) {
        // Usamos 'current' como base de la operación, NO 'this'
        RealNode currentNode = (RealNode) current;  // Asumiendo que sí o sí llega un RealNode
        RealNode newRealNode = (RealNode) newNode;

        // Insertar el nuevo nodo a la izquierda (prev) de currentNode
        newRealNode.next = currentNode;
        newRealNode.prev = currentNode.prev;
        currentNode.prev.next = newRealNode;
        currentNode.prev = newRealNode;

        // Retornamos el nodo que acabamos de agregar, aunque en tu diseño
        // podrías querer retornar el 'currentNode' en lugar de newRealNode.
        return newRealNode;
    }

    @Override
    public AbstractNode remove(AbstractNode node) {
        // Eliminamos el 'node' del anillo y devolvemos su siguiente
        RealNode currentNode = (RealNode) node;  // El nodo a eliminar
        RealNode nextNode = currentNode.next;    // El que le sigue, para devolverlo

        currentNode.prev.next = currentNode.next;
        currentNode.next.prev = currentNode.prev;

        return nextNode;  // Por convención, devolvemos el "nuevo current"
    }

    @Override
    public AbstractNode next(AbstractNode node) {
        // Devuelve el siguiente de 'node'
        RealNode currentNode = (RealNode) node;
        return currentNode.next;
    }

    @Override
    public Object current(AbstractNode node) {
        // Retorna el propio 'node' como "nodo actual"
        RealNode currentNode = (RealNode) node;
        return currentNode.cargo;
    }

}
