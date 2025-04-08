package anillo;

import java.util.Stack;

public class Ring {
    public AbstractNode current;
    // Tiene que tener una pila de AbstractNode
    private final Stack<AbstractNode> stack;

    public Ring() {
        // Inicializa la pila
        this.stack = new Stack<>();
        // Agrega un NullNode al stack
        this.stack.push(new NullNode());
    }

    public Ring next() {
        this.current = this.stack.lastElement().next(this.current);
        return this;
    }

    public Object current() {
        return this.stack.lastElement().current(this.current);
    }

    public Ring add(Object cargo) {
        RealNode node = new RealNode(cargo);
        //Llamar al metodo del ultimo del stack
        this.stack.lastElement().add(node,this.current);
        // lo agrego al stack
        this.stack.push(node);
        this.current = node;
        return this;


    }

    public Ring remove() {
        AbstractNode node = this.stack.pop();
        this.current = node.remove(this.current);
        return this;

    }
}


