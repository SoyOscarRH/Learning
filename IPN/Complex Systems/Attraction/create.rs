#[derive(Clone, Copy)]
pub enum Cell {
    Dead = 0,
    Alive = 1,
}

#[derive(Clone)]
pub struct Universe {
    width: usize,
    height: usize,
    cells: Vec<Cell>,
    survive_min: u8,
    survive_max: u8,
    birth_min: u8,
    birth_max: u8,
}

impl Universe {
    fn get(&self, row: usize, col: usize) -> Cell {
        let index = (row * self.width + col) as usize;
        self.cells[index]
    }

    fn count_alive_around(&self, row: usize, col: usize) -> u8 {
        let mut count = 0;
        for delta_row in &[0, 1, self.height - 1] {
            for delta_col in &[0, 1, self.width - 1] {
                let neighbor_row = (row + delta_row) % self.height;
                let neighbor_col = (col + delta_col) % self.width;
                count += self.get(neighbor_row, neighbor_col) as u8;
            }
        }

        count - self.get(row, col) as u8
    }

    pub fn tick(&mut self) {
        let mut next = Vec::with_capacity(self.width * self.height);

        for row in 0..self.height {
            for col in 0..self.width {
                let cell = self.get(row, col);
                let live_neighbors = self.count_alive_around(row, col);

                let next_cell = match (cell, live_neighbors) {
                    (Cell::Alive, neighbors) if neighbors < self.survive_min => Cell::Dead,
                    (Cell::Alive, neighbors) if neighbors > self.survive_max => Cell::Dead,
                    (Cell::Alive, _) => Cell::Alive,

                    (Cell::Dead, neighbors) if neighbors < self.birth_min => Cell::Dead,
                    (Cell::Dead, neighbors) if neighbors > self.birth_max => Cell::Dead,
                    (Cell::Dead, _) => Cell::Alive,
                };

                next.push(next_cell);
            }
        }

        self.cells = next;
    }

    pub fn print(&mut self) {
        for row in 0..self.height {
            for col in 0..self.width {
                match self.get(row, col) {
                    Cell::Alive => print!("◾"),
                    Cell::Dead => print!("◽"),
                };
            }
            println!("");
        }
    }
}

static WIDTH: usize = 4;
static HEIGHT: usize = 4;
static SIZE: usize = WIDTH * HEIGHT;

fn connects_to(id: u32, just_show: bool) {
    let mut cells = vec![Cell::Dead; SIZE];
    for n in 0..SIZE {
        if id >> n & 1 == 1 { cells[SIZE - n - 1] = Cell::Alive }
    }

    let mut universe = Universe {
        cells,
        width: WIDTH,
        height: HEIGHT,
        survive_min: 2,
        survive_max: 3,
        birth_min: 3,
        birth_max: 3,
    };

    if just_show {
        println!("{}", id);
        universe.print();
        return;
    }

    universe.tick();
    let mut new_id: u32 = 0;
    for n in 0..SIZE {
        new_id = match universe.cells[SIZE - n - 1] {
            Cell::Alive => new_id | 1 << n,
            Cell::Dead  => new_id | 0 << n,
        };
    }
    println!("G.add_edge({}, {})", id, new_id);
}

fn main() {
    println!("import networkx as nx");
    println!("from netwulf import visualize\n");
    println!("G = nx.Graph()");

    let limit = 1 << SIZE; 
    for i in 0..limit {
        connects_to(i, false);
    }

    println!("\nvisualize(G)");
}

