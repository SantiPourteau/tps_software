package juego.anillo;

import juego.jugador.Jugador;

public abstract class AbstractNode {
    public abstract AbstractNode add(AbstractNode node, AbstractNode current);
    public abstract AbstractNode remove(AbstractNode node);
    public abstract AbstractNode next(AbstractNode node);
    public abstract Jugador current(AbstractNode node);
    public abstract AbstractNode previous(AbstractNode node);}

