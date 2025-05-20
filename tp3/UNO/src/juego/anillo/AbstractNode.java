package juego.anillo;

public abstract class AbstractNode {
    public abstract AbstractNode add(AbstractNode node, AbstractNode current);
    public abstract AbstractNode remove(AbstractNode node);
    public abstract AbstractNode next(AbstractNode node);
    public abstract Object current(AbstractNode node); }
