package juego.controlador;

import java.util.List;
import juego.anillo.Ring;

/**
 * Controlador que avanza en sentido horario (derecha).
 */
public class ControladorDerecha extends Controlador {
    public ControladorDerecha(List<String> jugadores) {
        super(jugadores);
    }
    protected ControladorDerecha(Ring anillo) {
        super(anillo);
    }

    @Override
    public Controlador avanzar() {
        anillo.next();
        return this;
    }

    @Override
    public Controlador invertir() {
        return new ControladorIzquierda(anillo);
    }

    @Override
    public Controlador saltear() {
        anillo.next();
        anillo.next();
        return this;
    }
}
