// -*-c++-*-
#ifndef PLANT_PLANT_PARAMETERS_H_
#define PLANT_PLANT_PARAMETERS_H_

#include <vector>
#include <RcppCommon.h> // SEXP

#include <plant/control.h>
#include <plant/models/ff16_strategy.h>
#include <plant/node_schedule.h>
#include <plant/scm_utils.h> // Unfortunately needed for setup_node_schedule

#include <plant/disturbance_regime.h>
#include <plant/disturbances/no_disturbance.h>
#include <plant/disturbances/weibull_disturbance.h>

// TODO: I will possibly move out the "Patch" parameters out into
// their own simple list class at some point, to make this a bit more
// coherent.
//
// TODO: Will require some free functions on the R side:
//   * add_strategy (with flag for mutant/non mutant)

namespace plant {

template <typename T, typename E>
struct Parameters {
  typedef T strategy_type;
  typedef E environment_type;

  Parameters() :
    patch_area(1.0),
    n_patches(1),
    patch_type("meta-population"),
    max_patch_lifetime(105.32) // designed to agree with Daniel's implementation
  {
    validate();
  }

  // Data -- public for now (see github issue #17).
  double patch_area; // Size of the patch (m^2)
  size_t n_patches;  // Number of patches in the metacommunity
  std::string patch_type;
  double max_patch_lifetime; // Disturbance interval (years)
  std::vector<strategy_type> strategies;

  Disturbance_Regime* disturbance;

  // Default strategy.
  strategy_type strategy_default;

  // Node information.
  std::vector<double> node_schedule_times_default;
  std::vector<std::vector<double> > node_schedule_times;
  std::vector<double> ode_times;

  // Some little query functions for use on the C side:
  size_t size() const;
  void validate();

private:
  void setup_node_schedule();
};

template <typename T, typename E>
size_t Parameters<T,E>::size() const {
  return strategies.size();
}

// NOTE: this will be called *every time* that the object is passed in
// from R -> C++.  That's unlikely to be that often, but it does incur
// a penalty.  So don't put anything too stupidly heavy in here.
template <typename T, typename E>
void Parameters<T,E>::validate() {
  const size_t n_spp = size();

  setup_node_schedule();
  if (node_schedule_times.size() != n_spp) {
    util::stop("Incorrect length node_schedule_times");
  }

  // Disturbances used to describe evolution of a metapopulation of patches
  // when calculating fitness, otherwise defaults to fixed-duration run without
  // disturbance
  if(patch_type == "meta-population") {
    disturbance = new Weibull_Disturbance_Regime(max_patch_lifetime);
  }
  else {
    disturbance = new No_Disturbance();
  }
}

// Separating this out just because it's a bit crap:
// TODO: Consider adding this to scm_utils.h perhaps?
template <typename T, typename E>
void Parameters<T,E>::setup_node_schedule() {
  node_schedule_times_default =
      plant::node_schedule_times_default(max_patch_lifetime);

  if ((node_schedule_times.empty() && size() > 0)) {
    node_schedule_times.clear();
    for (size_t i = 0; i < size(); ++i) {
      node_schedule_times.push_back(node_schedule_times_default);
    }
  }
}
}

#endif
