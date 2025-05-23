package juego.carta;

import juego.Juego;
import juego.controlador.Controlador;
import juego.jugador.Jugador;

import java.util.Deque;
import java.util.Map;

public abstract class Carta {
    public String color;

    public abstract boolean esCompatible(Carta c);

    public abstract Controlador actualizarControlador(Juego juego, Controlador c);

    public int numLevantarCartas(){ // Por defecto levantas 0
        return 0;
    };

    // Son los comportamientos mas comunes por defecto, se sobreescriben por clase solo las que son necesarias.
    protected boolean esCompatibleColor(Carta c){
        return this.color.equals(c.color);
    }

    protected boolean esCompatibleReverse(Carta C){
        return false;
    }

    protected boolean esCompatibleNumero(Carta c){
        return false;
    }

    protected boolean esCompatibleSkip(Carta c){
        return false;
    }

    protected boolean esCompatibleDraw2(Carta c){
        return false;
    }

    protected boolean esCompatibleWild(Carta c){
        return true;
    }

    protected Jugador LevantarCartas(Jugador j){
        //por defecto levantas 0 cartas
        return j;
    }

    public int penalizar() {
        return 0;
    }


}
