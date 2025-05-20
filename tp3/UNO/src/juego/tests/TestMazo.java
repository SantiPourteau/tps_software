package juego.tests;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class TestMazo {
    private Mazo mazo;
    private final List<String> SECUENCIA = Arrays.asList(
            "R1", "B5", "G9", "Y0",
            "RSkip", "BDraw2", "GReverse", "W"
    );

    @BeforeEach
    void setUp() {
        mazo = new Mazo(SECUENCIA);
    }

    @Test
    void testMazoInicializaConSecuenciaDeterminista() {
        List<String> orden = mazo.getCartasRestantes().stream()
                .map(Carta::toString)
                .collect(Collectors.toList());
        assertEquals(SECUENCIA, orden, "El mazo debe reflejar la secuencia inicial sin shuffle");
    }

    @Test
    void testRobarCartaReduceTamaño() {
        int antes = mazo.cartasRestantes().size();
        Carta c = mazo.robarCarta();
        assertEquals(SECUENCIA.get(0), c.toString());
        assertEquals(antes - 1, mazo.cartasRestantes().size(), "Robar carta debe reducir el mazo en uno");
    }
}
