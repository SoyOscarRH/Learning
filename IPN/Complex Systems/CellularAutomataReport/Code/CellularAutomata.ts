type bits = Uint8Array;

class CellularAutomata {
  readonly histogram: Array<number>;
  epoch: number;
  data: bits;
  buffer: bits;

  constructor(init: Array<number>) {
    this.data = new Uint8Array(new ArrayBuffer(init.length));
    this.buffer = new Uint8Array(new ArrayBuffer(init.length));
    this.histogram = [];
    this.epoch = 0;

    let ones = 0;
    for (let i = 0; i < this.data.length; ++i) {
      this.data[i] = init[i];
      if (this.data[i]) ++ones;
    }

    this.histogram[this.epoch] = ones;
  }

  newEpoch(rulesID: number): void {
    this.epoch += 1;
    let ones = 0;

    const rule = this.getRules(rulesID);
    for (let i = 0; i < this.buffer.length; ++i) {
      this.buffer[i] = rule(this.data, i);
      if (this.buffer[i]) ++ones;
    }

    this.histogram[this.epoch] = ones;
    [this.data, this.buffer] = [this.buffer, this.data];
  }

  createNewEpoch(rulesID: number): bits {
    const rule = this.getRules(rulesID);
    return this.data.map((_, i) => rule(this.data, i));
  }

  getRules = (rulesID: number) => (data: bits, index: number): number => {
    const limit = data.length - 1;
    const n1 = index === 0 ? limit : index - 1;
    const n2 = index === limit ? 0 : index + 1;

    const id = (data[n1] << 2) + (data[index] << 1) + (data[n2] << 0);
    return (rulesID >> id) & 1;
  };

  get average(): number {
    let total = 0;
    for (let i = 0; i < this.histogram.length; ++i) total += this.histogram[i];

    return total / this.histogram.length;
  }

  get variance(): number {
    const average = this.average;
    let variance = 0;

    for (let i = 0; i < this.histogram.length; ++i)
      variance += (this.histogram[i] - average) * (this.histogram[i] - average);

    return variance / this.histogram.length;
  }
}

export default CellularAutomata;
