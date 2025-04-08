package anillo;

public interface AbstractNode {
    AbstractNode add(AbstractNode node, AbstractNode current);
    AbstractNode remove(AbstractNode node);
    AbstractNode next(AbstractNode node);
    Object current(AbstractNode node);

}
