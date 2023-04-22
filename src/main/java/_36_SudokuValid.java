// copilot solution :D
public class _36_SudokuValid {

    private boolean process(char c, boolean[] m) {
        if (c == '.') {
            return true;
        }
        if (m[c - '1']) {
            return false;
        }
        m[c - '1'] = true;
        return true;
    }

    public boolean isValidSudoku(char[][] board) {
        for (int i = 0; i < 9; i++) {
            boolean[] m = new boolean[9];
            for (int j = 0; j < 9; j++) {
                if (!process(board[i][j], m)) {
                    return false;
                }
            }
        }

        for (int j = 0; j < 9; j++) {
            boolean[] m = new boolean[9];
            for (int i = 0; i < 9; i++) {
                if (!process(board[i][j], m)) {
                    return false;
                }
            }
        }

        for (int block = 0; block < 9; block++) {
            boolean[] m = new boolean[9];
            for (int i = block / 3 * 3; i < block / 3 * 3 + 3; i++) {
                for (int j = block % 3 * 3; j < block % 3 * 3 + 3; j++) {
                    if (!process(board[i][j], m)) {
                        return false;
                    }
                }
            }
        }
        return true;
    }
}
