#include "calculator.h"

int* sum_1_svc(input* argp, struct svc_req* rqstp) {
  static int result;
  result = argp->input_1 + argp->input_2;
  return &result;
}

int* substract_1_svc(input* argp, struct svc_req* rqstp) {
  static int result;
  result = argp->input_1 - argp->input_2;
  return &result;
}

int* multiply_1_svc(input* argp, struct svc_req* rqstp) {
  static int result;
  result = argp->input_1 * argp->input_2;
  return &result;
}

float* divide_1_svc(input* argp, struct svc_req* rqstp) {
  static float result;
  result = (float) argp->input_1 / (float) argp->input_2;
  return &result;
}
