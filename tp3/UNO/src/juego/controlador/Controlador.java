package juego.controlador;

import juego.anillo.Ring;

import java.util.List;


public abstract class Controlador {
    private Ring anillo;
    public Controlador(List<String> jugadores) {
        //Inicialize la lista circula doblemente enlazada enlazando los jugadores
    }

    public abstract Controlador avanzar();

    public abstract Controlador invertir();

    public abstract Controlador saltear();

}

