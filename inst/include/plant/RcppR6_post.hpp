// Generated by RcppR6 (0.2.4): do not edit by hand
#ifndef _PLANT_RCPPR6_POST_HPP_
#define _PLANT_RCPPR6_POST_HPP_

#include <Rcpp.h>
#include <plant/RcppR6_support.hpp>

namespace plant {
namespace RcppR6 {
namespace traits {
template <> inline std::string   class_name_r<plant::ode::test::Lorenz >() {return "Lorenz";}
template <> inline std::string   package_name<plant::ode::test::Lorenz >() {return "plant";}
template <> inline std::string generator_name<plant::ode::test::Lorenz >() {return ".R6_Lorenz";}
template <> inline std::string   class_name_r<plant::ode::test::OdeR >() {return "OdeR";}
template <> inline std::string   package_name<plant::ode::test::OdeR >() {return "plant";}
template <> inline std::string generator_name<plant::ode::test::OdeR >() {return ".R6_OdeR";}
template <> inline std::string   class_name_r<plant::ode::Runner<plant::ode::test::Lorenz> >() {return "OdeRunner<Lorenz>";}
template <> inline std::string   package_name<plant::ode::Runner<plant::ode::test::Lorenz> >() {return "plant";}
template <> inline std::string generator_name<plant::ode::Runner<plant::ode::test::Lorenz> >() {return ".R6_OdeRunner___Lorenz";}

template <> inline std::string   class_name_r<plant::ode::Runner<plant::ode::test::OdeR> >() {return "OdeRunner<OdeR>";}
template <> inline std::string   package_name<plant::ode::Runner<plant::ode::test::OdeR> >() {return "plant";}
template <> inline std::string generator_name<plant::ode::Runner<plant::ode::test::OdeR> >() {return ".R6_OdeRunner___OdeR";}

template <> inline std::string   class_name_r<plant::ode::Runner<plant::tools::PlantRunner<plant::FF16_Strategy> > >() {return "OdeRunner<FF16>";}
template <> inline std::string   package_name<plant::ode::Runner<plant::tools::PlantRunner<plant::FF16_Strategy> > >() {return "plant";}
template <> inline std::string generator_name<plant::ode::Runner<plant::tools::PlantRunner<plant::FF16_Strategy> > >() {return ".R6_OdeRunner___FF16";}
template <> inline std::string   class_name_r<plant::CohortScheduleEvent >() {return "CohortScheduleEvent";}
template <> inline std::string   package_name<plant::CohortScheduleEvent >() {return "plant";}
template <> inline std::string generator_name<plant::CohortScheduleEvent >() {return ".R6_CohortScheduleEvent";}
template <> inline std::string   class_name_r<plant::CohortSchedule >() {return "CohortSchedule";}
template <> inline std::string   package_name<plant::CohortSchedule >() {return "plant";}
template <> inline std::string generator_name<plant::CohortSchedule >() {return ".R6_CohortSchedule";}
template <> inline std::string   class_name_r<plant::Disturbance >() {return "Disturbance";}
template <> inline std::string   package_name<plant::Disturbance >() {return "plant";}
template <> inline std::string generator_name<plant::Disturbance >() {return ".R6_Disturbance";}
template <> inline std::string   class_name_r<plant::Control >() {return "Control";}
template <> inline std::string   package_name<plant::Control >() {return "plant";}
template <> inline std::string generator_name<plant::Control >() {return "";}
template <> inline std::string   class_name_r<plant::ode::OdeControl >() {return "OdeControl";}
template <> inline std::string   package_name<plant::ode::OdeControl >() {return "plant";}
template <> inline std::string generator_name<plant::ode::OdeControl >() {return "";}
template <> inline std::string   class_name_r<plant::quadrature::QK >() {return "QK";}
template <> inline std::string   package_name<plant::quadrature::QK >() {return "plant";}
template <> inline std::string generator_name<plant::quadrature::QK >() {return ".R6_QK";}
template <> inline std::string   class_name_r<plant::quadrature::QAG >() {return "QAG";}
template <> inline std::string   package_name<plant::quadrature::QAG >() {return "plant";}
template <> inline std::string generator_name<plant::quadrature::QAG >() {return ".R6_QAG";}
template <> inline std::string   class_name_r<plant::interpolator::Interpolator >() {return "Interpolator";}
template <> inline std::string   package_name<plant::interpolator::Interpolator >() {return "plant";}
template <> inline std::string generator_name<plant::interpolator::Interpolator >() {return ".R6_Interpolator";}
template <> inline std::string   class_name_r<plant::Environment >() {return "Environment";}
template <> inline std::string   package_name<plant::Environment >() {return "plant";}
template <> inline std::string generator_name<plant::Environment >() {return ".R6_Environment";}
template <> inline std::string   class_name_r<plant::Plant<plant::FF16_Strategy> >() {return "Plant<FF16>";}
template <> inline std::string   package_name<plant::Plant<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::Plant<plant::FF16_Strategy> >() {return ".R6_Plant___FF16";}
template <> inline std::string   class_name_r<plant::tools::PlantRunner<plant::FF16_Strategy> >() {return "PlantRunner<FF16>";}
template <> inline std::string   package_name<plant::tools::PlantRunner<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::tools::PlantRunner<plant::FF16_Strategy> >() {return ".R6_PlantRunner___FF16";}
template <> inline std::string   class_name_r<plant::FF16_Strategy >() {return "FF16_Strategy";}
template <> inline std::string   package_name<plant::FF16_Strategy >() {return "plant";}
template <> inline std::string generator_name<plant::FF16_Strategy >() {return "";}
template <> inline std::string   class_name_r<plant::Internals >() {return "Internals";}
template <> inline std::string   package_name<plant::Internals >() {return "plant";}
template <> inline std::string generator_name<plant::Internals >() {return "";}
template <> inline std::string   class_name_r<plant::Parameters<plant::FF16_Strategy> >() {return "Parameters<FF16>";}
template <> inline std::string   package_name<plant::Parameters<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::Parameters<plant::FF16_Strategy> >() {return "";}
template <> inline std::string   class_name_r<plant::Cohort<plant::FF16_Strategy> >() {return "Cohort<FF16>";}
template <> inline std::string   package_name<plant::Cohort<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::Cohort<plant::FF16_Strategy> >() {return ".R6_Cohort___FF16";}
template <> inline std::string   class_name_r<plant::Species<plant::FF16_Strategy> >() {return "Species<FF16>";}
template <> inline std::string   package_name<plant::Species<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::Species<plant::FF16_Strategy> >() {return ".R6_Species___FF16";}
template <> inline std::string   class_name_r<plant::Patch<plant::FF16_Strategy> >() {return "Patch<FF16>";}
template <> inline std::string   package_name<plant::Patch<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::Patch<plant::FF16_Strategy> >() {return ".R6_Patch___FF16";}
template <> inline std::string   class_name_r<plant::SCM<plant::FF16_Strategy> >() {return "SCM<FF16>";}
template <> inline std::string   package_name<plant::SCM<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::SCM<plant::FF16_Strategy> >() {return ".R6_SCM___FF16";}
template <> inline std::string   class_name_r<plant::StochasticSpecies<plant::FF16_Strategy> >() {return "StochasticSpecies<FF16>";}
template <> inline std::string   package_name<plant::StochasticSpecies<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::StochasticSpecies<plant::FF16_Strategy> >() {return ".R6_StochasticSpecies___FF16";}
template <> inline std::string   class_name_r<plant::StochasticPatch<plant::FF16_Strategy> >() {return "StochasticPatch<FF16>";}
template <> inline std::string   package_name<plant::StochasticPatch<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::StochasticPatch<plant::FF16_Strategy> >() {return ".R6_StochasticPatch___FF16";}
template <> inline std::string   class_name_r<plant::StochasticPatchRunner<plant::FF16_Strategy> >() {return "StochasticPatchRunner<FF16>";}
template <> inline std::string   package_name<plant::StochasticPatchRunner<plant::FF16_Strategy> >() {return "plant";}
template <> inline std::string generator_name<plant::StochasticPatchRunner<plant::FF16_Strategy> >() {return ".R6_StochasticPatchRunner___FF16";}
}
}
}

namespace Rcpp {
template <typename T>
SEXP wrap(const plant::RcppR6::RcppR6<T>& x) {
  return x.to_R6();
}

namespace traits {
template <typename T>
class Exporter<plant::RcppR6::RcppR6<T> > {
public:
  Exporter(SEXP x) : obj(plant::RcppR6::RcppR6<T>(x)) {}
  inline plant::RcppR6::RcppR6<T> get() { return obj; }
private:
  plant::RcppR6::RcppR6<T> obj;
};
}

template <> inline SEXP wrap(const plant::ode::test::Lorenz& x) {
  return wrap(plant::RcppR6::RcppR6<plant::ode::test::Lorenz>(x));
}
template <> inline plant::ode::test::Lorenz as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::ode::test::Lorenz>(x));
}
template <> inline SEXP wrap(const plant::ode::test::OdeR& x) {
  return wrap(plant::RcppR6::RcppR6<plant::ode::test::OdeR>(x));
}
template <> inline plant::ode::test::OdeR as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::ode::test::OdeR>(x));
}
template <> inline SEXP wrap(const plant::ode::Runner<plant::ode::test::Lorenz>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::ode::Runner<plant::ode::test::Lorenz> >(x));
}
template <> inline plant::ode::Runner<plant::ode::test::Lorenz> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::ode::Runner<plant::ode::test::Lorenz> >(x));
}

template <> inline SEXP wrap(const plant::ode::Runner<plant::ode::test::OdeR>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::ode::Runner<plant::ode::test::OdeR> >(x));
}
template <> inline plant::ode::Runner<plant::ode::test::OdeR> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::ode::Runner<plant::ode::test::OdeR> >(x));
}

template <> inline SEXP wrap(const plant::ode::Runner<plant::tools::PlantRunner<plant::FF16_Strategy> >& x) {
  return wrap(plant::RcppR6::RcppR6<plant::ode::Runner<plant::tools::PlantRunner<plant::FF16_Strategy> > >(x));
}
template <> inline plant::ode::Runner<plant::tools::PlantRunner<plant::FF16_Strategy> > as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::ode::Runner<plant::tools::PlantRunner<plant::FF16_Strategy> > >(x));
}
template <> inline SEXP wrap(const plant::CohortScheduleEvent& x) {
  return wrap(plant::RcppR6::RcppR6<plant::CohortScheduleEvent>(x));
}
template <> inline plant::CohortScheduleEvent as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::CohortScheduleEvent>(x));
}
template <> inline SEXP wrap(const plant::CohortSchedule& x) {
  return wrap(plant::RcppR6::RcppR6<plant::CohortSchedule>(x));
}
template <> inline plant::CohortSchedule as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::CohortSchedule>(x));
}
template <> inline SEXP wrap(const plant::Disturbance& x) {
  return wrap(plant::RcppR6::RcppR6<plant::Disturbance>(x));
}
template <> inline plant::Disturbance as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::Disturbance>(x));
}
template <> inline SEXP wrap(const plant::Control& x) {
  Rcpp::List ret;
  ret["plant_assimilation_adaptive"] = Rcpp::wrap(x.plant_assimilation_adaptive);
  ret["plant_assimilation_over_distribution"] = Rcpp::wrap(x.plant_assimilation_over_distribution);
  ret["plant_assimilation_tol"] = Rcpp::wrap(x.plant_assimilation_tol);
  ret["plant_assimilation_iterations"] = Rcpp::wrap(x.plant_assimilation_iterations);
  ret["plant_assimilation_rule"] = Rcpp::wrap(x.plant_assimilation_rule);
  ret["plant_seed_tol"] = Rcpp::wrap(x.plant_seed_tol);
  ret["plant_seed_iterations"] = Rcpp::wrap(x.plant_seed_iterations);
  ret["cohort_gradient_eps"] = Rcpp::wrap(x.cohort_gradient_eps);
  ret["cohort_gradient_direction"] = Rcpp::wrap(x.cohort_gradient_direction);
  ret["cohort_gradient_richardson"] = Rcpp::wrap(x.cohort_gradient_richardson);
  ret["cohort_gradient_richardson_depth"] = Rcpp::wrap(x.cohort_gradient_richardson_depth);
  ret["environment_light_tol"] = Rcpp::wrap(x.environment_light_tol);
  ret["environment_light_nbase"] = Rcpp::wrap(x.environment_light_nbase);
  ret["environment_light_max_depth"] = Rcpp::wrap(x.environment_light_max_depth);
  ret["environment_light_rescale_usually"] = Rcpp::wrap(x.environment_light_rescale_usually);
  ret["ode_step_size_initial"] = Rcpp::wrap(x.ode_step_size_initial);
  ret["ode_step_size_min"] = Rcpp::wrap(x.ode_step_size_min);
  ret["ode_step_size_max"] = Rcpp::wrap(x.ode_step_size_max);
  ret["ode_tol_rel"] = Rcpp::wrap(x.ode_tol_rel);
  ret["ode_tol_abs"] = Rcpp::wrap(x.ode_tol_abs);
  ret["ode_a_y"] = Rcpp::wrap(x.ode_a_y);
  ret["ode_a_dydt"] = Rcpp::wrap(x.ode_a_dydt);
  ret["schedule_nsteps"] = Rcpp::wrap(x.schedule_nsteps);
  ret["schedule_eps"] = Rcpp::wrap(x.schedule_eps);
  ret["schedule_verbose"] = Rcpp::wrap(x.schedule_verbose);
  ret["schedule_patch_survival"] = Rcpp::wrap(x.schedule_patch_survival);
  ret["equilibrium_nsteps"] = Rcpp::wrap(x.equilibrium_nsteps);
  ret["equilibrium_eps"] = Rcpp::wrap(x.equilibrium_eps);
  ret["equilibrium_large_seed_rain_change"] = Rcpp::wrap(x.equilibrium_large_seed_rain_change);
  ret["equilibrium_verbose"] = Rcpp::wrap(x.equilibrium_verbose);
  ret["equilibrium_solver_name"] = Rcpp::wrap(x.equilibrium_solver_name);
  ret["equilibrium_extinct_seed_rain"] = Rcpp::wrap(x.equilibrium_extinct_seed_rain);
  ret["equilibrium_nattempts"] = Rcpp::wrap(x.equilibrium_nattempts);
  ret["equilibrium_solver_logN"] = Rcpp::wrap(x.equilibrium_solver_logN);
  ret["equilibrium_solver_try_keep"] = Rcpp::wrap(x.equilibrium_solver_try_keep);
  ret.attr("class") = "Control";
  return ret;
}
template <> inline plant::Control as(SEXP x) {
  if (!plant::RcppR6::is<plant::Control >(x)) {
    Rcpp::stop("Expected an object of type Control");
    // NOTE: Won't drop through or return anything.
  }
  // NOTE: assumes default constructable, and will assign *every*
  // field twice.  No current support for a hook.
  plant::Control ret;
  Rcpp::List xl(x);
  // ret.plant_assimilation_adaptive = Rcpp::as<decltype(retplant_assimilation_adaptive) >(xl["plant_assimilation_adaptive"]);
  ret.plant_assimilation_adaptive = Rcpp::as<bool >(xl["plant_assimilation_adaptive"]);
  // ret.plant_assimilation_over_distribution = Rcpp::as<decltype(retplant_assimilation_over_distribution) >(xl["plant_assimilation_over_distribution"]);
  ret.plant_assimilation_over_distribution = Rcpp::as<bool >(xl["plant_assimilation_over_distribution"]);
  // ret.plant_assimilation_tol = Rcpp::as<decltype(retplant_assimilation_tol) >(xl["plant_assimilation_tol"]);
  ret.plant_assimilation_tol = Rcpp::as<double >(xl["plant_assimilation_tol"]);
  // ret.plant_assimilation_iterations = Rcpp::as<decltype(retplant_assimilation_iterations) >(xl["plant_assimilation_iterations"]);
  ret.plant_assimilation_iterations = Rcpp::as<size_t >(xl["plant_assimilation_iterations"]);
  // ret.plant_assimilation_rule = Rcpp::as<decltype(retplant_assimilation_rule) >(xl["plant_assimilation_rule"]);
  ret.plant_assimilation_rule = Rcpp::as<size_t >(xl["plant_assimilation_rule"]);
  // ret.plant_seed_tol = Rcpp::as<decltype(retplant_seed_tol) >(xl["plant_seed_tol"]);
  ret.plant_seed_tol = Rcpp::as<double >(xl["plant_seed_tol"]);
  // ret.plant_seed_iterations = Rcpp::as<decltype(retplant_seed_iterations) >(xl["plant_seed_iterations"]);
  ret.plant_seed_iterations = Rcpp::as<int >(xl["plant_seed_iterations"]);
  // ret.cohort_gradient_eps = Rcpp::as<decltype(retcohort_gradient_eps) >(xl["cohort_gradient_eps"]);
  ret.cohort_gradient_eps = Rcpp::as<double >(xl["cohort_gradient_eps"]);
  // ret.cohort_gradient_direction = Rcpp::as<decltype(retcohort_gradient_direction) >(xl["cohort_gradient_direction"]);
  ret.cohort_gradient_direction = Rcpp::as<int >(xl["cohort_gradient_direction"]);
  // ret.cohort_gradient_richardson = Rcpp::as<decltype(retcohort_gradient_richardson) >(xl["cohort_gradient_richardson"]);
  ret.cohort_gradient_richardson = Rcpp::as<bool >(xl["cohort_gradient_richardson"]);
  // ret.cohort_gradient_richardson_depth = Rcpp::as<decltype(retcohort_gradient_richardson_depth) >(xl["cohort_gradient_richardson_depth"]);
  ret.cohort_gradient_richardson_depth = Rcpp::as<size_t >(xl["cohort_gradient_richardson_depth"]);
  // ret.environment_light_tol = Rcpp::as<decltype(retenvironment_light_tol) >(xl["environment_light_tol"]);
  ret.environment_light_tol = Rcpp::as<double >(xl["environment_light_tol"]);
  // ret.environment_light_nbase = Rcpp::as<decltype(retenvironment_light_nbase) >(xl["environment_light_nbase"]);
  ret.environment_light_nbase = Rcpp::as<size_t >(xl["environment_light_nbase"]);
  // ret.environment_light_max_depth = Rcpp::as<decltype(retenvironment_light_max_depth) >(xl["environment_light_max_depth"]);
  ret.environment_light_max_depth = Rcpp::as<size_t >(xl["environment_light_max_depth"]);
  // ret.environment_light_rescale_usually = Rcpp::as<decltype(retenvironment_light_rescale_usually) >(xl["environment_light_rescale_usually"]);
  ret.environment_light_rescale_usually = Rcpp::as<bool >(xl["environment_light_rescale_usually"]);
  // ret.ode_step_size_initial = Rcpp::as<decltype(retode_step_size_initial) >(xl["ode_step_size_initial"]);
  ret.ode_step_size_initial = Rcpp::as<double >(xl["ode_step_size_initial"]);
  // ret.ode_step_size_min = Rcpp::as<decltype(retode_step_size_min) >(xl["ode_step_size_min"]);
  ret.ode_step_size_min = Rcpp::as<double >(xl["ode_step_size_min"]);
  // ret.ode_step_size_max = Rcpp::as<decltype(retode_step_size_max) >(xl["ode_step_size_max"]);
  ret.ode_step_size_max = Rcpp::as<double >(xl["ode_step_size_max"]);
  // ret.ode_tol_rel = Rcpp::as<decltype(retode_tol_rel) >(xl["ode_tol_rel"]);
  ret.ode_tol_rel = Rcpp::as<double >(xl["ode_tol_rel"]);
  // ret.ode_tol_abs = Rcpp::as<decltype(retode_tol_abs) >(xl["ode_tol_abs"]);
  ret.ode_tol_abs = Rcpp::as<double >(xl["ode_tol_abs"]);
  // ret.ode_a_y = Rcpp::as<decltype(retode_a_y) >(xl["ode_a_y"]);
  ret.ode_a_y = Rcpp::as<double >(xl["ode_a_y"]);
  // ret.ode_a_dydt = Rcpp::as<decltype(retode_a_dydt) >(xl["ode_a_dydt"]);
  ret.ode_a_dydt = Rcpp::as<double >(xl["ode_a_dydt"]);
  // ret.schedule_nsteps = Rcpp::as<decltype(retschedule_nsteps) >(xl["schedule_nsteps"]);
  ret.schedule_nsteps = Rcpp::as<size_t >(xl["schedule_nsteps"]);
  // ret.schedule_eps = Rcpp::as<decltype(retschedule_eps) >(xl["schedule_eps"]);
  ret.schedule_eps = Rcpp::as<double >(xl["schedule_eps"]);
  // ret.schedule_verbose = Rcpp::as<decltype(retschedule_verbose) >(xl["schedule_verbose"]);
  ret.schedule_verbose = Rcpp::as<bool >(xl["schedule_verbose"]);
  // ret.schedule_patch_survival = Rcpp::as<decltype(retschedule_patch_survival) >(xl["schedule_patch_survival"]);
  ret.schedule_patch_survival = Rcpp::as<double >(xl["schedule_patch_survival"]);
  // ret.equilibrium_nsteps = Rcpp::as<decltype(retequilibrium_nsteps) >(xl["equilibrium_nsteps"]);
  ret.equilibrium_nsteps = Rcpp::as<size_t >(xl["equilibrium_nsteps"]);
  // ret.equilibrium_eps = Rcpp::as<decltype(retequilibrium_eps) >(xl["equilibrium_eps"]);
  ret.equilibrium_eps = Rcpp::as<double >(xl["equilibrium_eps"]);
  // ret.equilibrium_large_seed_rain_change = Rcpp::as<decltype(retequilibrium_large_seed_rain_change) >(xl["equilibrium_large_seed_rain_change"]);
  ret.equilibrium_large_seed_rain_change = Rcpp::as<double >(xl["equilibrium_large_seed_rain_change"]);
  // ret.equilibrium_verbose = Rcpp::as<decltype(retequilibrium_verbose) >(xl["equilibrium_verbose"]);
  ret.equilibrium_verbose = Rcpp::as<bool >(xl["equilibrium_verbose"]);
  // ret.equilibrium_solver_name = Rcpp::as<decltype(retequilibrium_solver_name) >(xl["equilibrium_solver_name"]);
  ret.equilibrium_solver_name = Rcpp::as<std::string >(xl["equilibrium_solver_name"]);
  // ret.equilibrium_extinct_seed_rain = Rcpp::as<decltype(retequilibrium_extinct_seed_rain) >(xl["equilibrium_extinct_seed_rain"]);
  ret.equilibrium_extinct_seed_rain = Rcpp::as<double >(xl["equilibrium_extinct_seed_rain"]);
  // ret.equilibrium_nattempts = Rcpp::as<decltype(retequilibrium_nattempts) >(xl["equilibrium_nattempts"]);
  ret.equilibrium_nattempts = Rcpp::as<int >(xl["equilibrium_nattempts"]);
  // ret.equilibrium_solver_logN = Rcpp::as<decltype(retequilibrium_solver_logN) >(xl["equilibrium_solver_logN"]);
  ret.equilibrium_solver_logN = Rcpp::as<bool >(xl["equilibrium_solver_logN"]);
  // ret.equilibrium_solver_try_keep = Rcpp::as<decltype(retequilibrium_solver_try_keep) >(xl["equilibrium_solver_try_keep"]);
  ret.equilibrium_solver_try_keep = Rcpp::as<bool >(xl["equilibrium_solver_try_keep"]);
  return ret;
}
template <> inline SEXP wrap(const plant::ode::OdeControl& x) {
  Rcpp::List ret;
  ret["tol_abs"] = Rcpp::wrap(x.tol_abs);
  ret["tol_rel"] = Rcpp::wrap(x.tol_rel);
  ret["a_y"] = Rcpp::wrap(x.a_y);
  ret["a_dydt"] = Rcpp::wrap(x.a_dydt);
  ret["step_size_min"] = Rcpp::wrap(x.step_size_min);
  ret["step_size_max"] = Rcpp::wrap(x.step_size_max);
  ret["step_size_initial"] = Rcpp::wrap(x.step_size_initial);
  ret.attr("class") = "OdeControl";
  return ret;
}
template <> inline plant::ode::OdeControl as(SEXP x) {
  if (!plant::RcppR6::is<plant::ode::OdeControl >(x)) {
    Rcpp::stop("Expected an object of type OdeControl");
    // NOTE: Won't drop through or return anything.
  }
  // NOTE: assumes default constructable, and will assign *every*
  // field twice.  No current support for a hook.
  plant::ode::OdeControl ret;
  Rcpp::List xl(x);
  // ret.tol_abs = Rcpp::as<decltype(rettol_abs) >(xl["tol_abs"]);
  ret.tol_abs = Rcpp::as<double >(xl["tol_abs"]);
  // ret.tol_rel = Rcpp::as<decltype(rettol_rel) >(xl["tol_rel"]);
  ret.tol_rel = Rcpp::as<double >(xl["tol_rel"]);
  // ret.a_y = Rcpp::as<decltype(reta_y) >(xl["a_y"]);
  ret.a_y = Rcpp::as<double >(xl["a_y"]);
  // ret.a_dydt = Rcpp::as<decltype(reta_dydt) >(xl["a_dydt"]);
  ret.a_dydt = Rcpp::as<double >(xl["a_dydt"]);
  // ret.step_size_min = Rcpp::as<decltype(retstep_size_min) >(xl["step_size_min"]);
  ret.step_size_min = Rcpp::as<double >(xl["step_size_min"]);
  // ret.step_size_max = Rcpp::as<decltype(retstep_size_max) >(xl["step_size_max"]);
  ret.step_size_max = Rcpp::as<double >(xl["step_size_max"]);
  // ret.step_size_initial = Rcpp::as<decltype(retstep_size_initial) >(xl["step_size_initial"]);
  ret.step_size_initial = Rcpp::as<double >(xl["step_size_initial"]);
  return ret;
}
template <> inline SEXP wrap(const plant::quadrature::QK& x) {
  return wrap(plant::RcppR6::RcppR6<plant::quadrature::QK>(x));
}
template <> inline plant::quadrature::QK as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::quadrature::QK>(x));
}
template <> inline SEXP wrap(const plant::quadrature::QAG& x) {
  return wrap(plant::RcppR6::RcppR6<plant::quadrature::QAG>(x));
}
template <> inline plant::quadrature::QAG as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::quadrature::QAG>(x));
}
template <> inline SEXP wrap(const plant::interpolator::Interpolator& x) {
  return wrap(plant::RcppR6::RcppR6<plant::interpolator::Interpolator>(x));
}
template <> inline plant::interpolator::Interpolator as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::interpolator::Interpolator>(x));
}
template <> inline SEXP wrap(const plant::Environment& x) {
  return wrap(plant::RcppR6::RcppR6<plant::Environment>(x));
}
template <> inline plant::Environment as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::Environment>(x));
}
template <> inline SEXP wrap(const plant::Plant<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::Plant<plant::FF16_Strategy> >(x));
}
template <> inline plant::Plant<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::Plant<plant::FF16_Strategy> >(x));
}
template <> inline SEXP wrap(const plant::tools::PlantRunner<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::tools::PlantRunner<plant::FF16_Strategy> >(x));
}
template <> inline plant::tools::PlantRunner<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::tools::PlantRunner<plant::FF16_Strategy> >(x));
}
template <> inline SEXP wrap(const plant::FF16_Strategy& x) {
  Rcpp::List ret;
  ret["lma"] = Rcpp::wrap(x.lma);
  ret["rho"] = Rcpp::wrap(x.rho);
  ret["hmat"] = Rcpp::wrap(x.hmat);
  ret["omega"] = Rcpp::wrap(x.omega);
  ret["eta"] = Rcpp::wrap(x.eta);
  ret["theta"] = Rcpp::wrap(x.theta);
  ret["a_l1"] = Rcpp::wrap(x.a_l1);
  ret["a_l2"] = Rcpp::wrap(x.a_l2);
  ret["a_r1"] = Rcpp::wrap(x.a_r1);
  ret["a_b1"] = Rcpp::wrap(x.a_b1);
  ret["r_s"] = Rcpp::wrap(x.r_s);
  ret["r_b"] = Rcpp::wrap(x.r_b);
  ret["r_r"] = Rcpp::wrap(x.r_r);
  ret["r_l"] = Rcpp::wrap(x.r_l);
  ret["a_y"] = Rcpp::wrap(x.a_y);
  ret["a_bio"] = Rcpp::wrap(x.a_bio);
  ret["k_l"] = Rcpp::wrap(x.k_l);
  ret["k_b"] = Rcpp::wrap(x.k_b);
  ret["k_s"] = Rcpp::wrap(x.k_s);
  ret["k_r"] = Rcpp::wrap(x.k_r);
  ret["a_p1"] = Rcpp::wrap(x.a_p1);
  ret["a_p2"] = Rcpp::wrap(x.a_p2);
  ret["a_f3"] = Rcpp::wrap(x.a_f3);
  ret["a_f1"] = Rcpp::wrap(x.a_f1);
  ret["a_f2"] = Rcpp::wrap(x.a_f2);
  ret["S_D"] = Rcpp::wrap(x.S_D);
  ret["a_d0"] = Rcpp::wrap(x.a_d0);
  ret["d_I"] = Rcpp::wrap(x.d_I);
  ret["a_dG1"] = Rcpp::wrap(x.a_dG1);
  ret["a_dG2"] = Rcpp::wrap(x.a_dG2);
  ret["control"] = Rcpp::wrap(x.control);
  ret.attr("class") = "FF16_Strategy";
  return ret;
}
template <> inline plant::FF16_Strategy as(SEXP x) {
  if (!plant::RcppR6::is<plant::FF16_Strategy >(x)) {
    Rcpp::stop("Expected an object of type FF16_Strategy");
    // NOTE: Won't drop through or return anything.
  }
  // NOTE: assumes default constructable, and will assign *every*
  // field twice.  No current support for a hook.
  plant::FF16_Strategy ret;
  Rcpp::List xl(x);
  // ret.lma = Rcpp::as<decltype(retlma) >(xl["lma"]);
  ret.lma = Rcpp::as<double >(xl["lma"]);
  // ret.rho = Rcpp::as<decltype(retrho) >(xl["rho"]);
  ret.rho = Rcpp::as<double >(xl["rho"]);
  // ret.hmat = Rcpp::as<decltype(rethmat) >(xl["hmat"]);
  ret.hmat = Rcpp::as<double >(xl["hmat"]);
  // ret.omega = Rcpp::as<decltype(retomega) >(xl["omega"]);
  ret.omega = Rcpp::as<double >(xl["omega"]);
  // ret.eta = Rcpp::as<decltype(reteta) >(xl["eta"]);
  ret.eta = Rcpp::as<double >(xl["eta"]);
  // ret.theta = Rcpp::as<decltype(rettheta) >(xl["theta"]);
  ret.theta = Rcpp::as<double >(xl["theta"]);
  // ret.a_l1 = Rcpp::as<decltype(reta_l1) >(xl["a_l1"]);
  ret.a_l1 = Rcpp::as<double >(xl["a_l1"]);
  // ret.a_l2 = Rcpp::as<decltype(reta_l2) >(xl["a_l2"]);
  ret.a_l2 = Rcpp::as<double >(xl["a_l2"]);
  // ret.a_r1 = Rcpp::as<decltype(reta_r1) >(xl["a_r1"]);
  ret.a_r1 = Rcpp::as<double >(xl["a_r1"]);
  // ret.a_b1 = Rcpp::as<decltype(reta_b1) >(xl["a_b1"]);
  ret.a_b1 = Rcpp::as<double >(xl["a_b1"]);
  // ret.r_s = Rcpp::as<decltype(retr_s) >(xl["r_s"]);
  ret.r_s = Rcpp::as<double >(xl["r_s"]);
  // ret.r_b = Rcpp::as<decltype(retr_b) >(xl["r_b"]);
  ret.r_b = Rcpp::as<double >(xl["r_b"]);
  // ret.r_r = Rcpp::as<decltype(retr_r) >(xl["r_r"]);
  ret.r_r = Rcpp::as<double >(xl["r_r"]);
  // ret.r_l = Rcpp::as<decltype(retr_l) >(xl["r_l"]);
  ret.r_l = Rcpp::as<double >(xl["r_l"]);
  // ret.a_y = Rcpp::as<decltype(reta_y) >(xl["a_y"]);
  ret.a_y = Rcpp::as<double >(xl["a_y"]);
  // ret.a_bio = Rcpp::as<decltype(reta_bio) >(xl["a_bio"]);
  ret.a_bio = Rcpp::as<double >(xl["a_bio"]);
  // ret.k_l = Rcpp::as<decltype(retk_l) >(xl["k_l"]);
  ret.k_l = Rcpp::as<double >(xl["k_l"]);
  // ret.k_b = Rcpp::as<decltype(retk_b) >(xl["k_b"]);
  ret.k_b = Rcpp::as<double >(xl["k_b"]);
  // ret.k_s = Rcpp::as<decltype(retk_s) >(xl["k_s"]);
  ret.k_s = Rcpp::as<double >(xl["k_s"]);
  // ret.k_r = Rcpp::as<decltype(retk_r) >(xl["k_r"]);
  ret.k_r = Rcpp::as<double >(xl["k_r"]);
  // ret.a_p1 = Rcpp::as<decltype(reta_p1) >(xl["a_p1"]);
  ret.a_p1 = Rcpp::as<double >(xl["a_p1"]);
  // ret.a_p2 = Rcpp::as<decltype(reta_p2) >(xl["a_p2"]);
  ret.a_p2 = Rcpp::as<double >(xl["a_p2"]);
  // ret.a_f3 = Rcpp::as<decltype(reta_f3) >(xl["a_f3"]);
  ret.a_f3 = Rcpp::as<double >(xl["a_f3"]);
  // ret.a_f1 = Rcpp::as<decltype(reta_f1) >(xl["a_f1"]);
  ret.a_f1 = Rcpp::as<double >(xl["a_f1"]);
  // ret.a_f2 = Rcpp::as<decltype(reta_f2) >(xl["a_f2"]);
  ret.a_f2 = Rcpp::as<double >(xl["a_f2"]);
  // ret.S_D = Rcpp::as<decltype(retS_D) >(xl["S_D"]);
  ret.S_D = Rcpp::as<double >(xl["S_D"]);
  // ret.a_d0 = Rcpp::as<decltype(reta_d0) >(xl["a_d0"]);
  ret.a_d0 = Rcpp::as<double >(xl["a_d0"]);
  // ret.d_I = Rcpp::as<decltype(retd_I) >(xl["d_I"]);
  ret.d_I = Rcpp::as<double >(xl["d_I"]);
  // ret.a_dG1 = Rcpp::as<decltype(reta_dG1) >(xl["a_dG1"]);
  ret.a_dG1 = Rcpp::as<double >(xl["a_dG1"]);
  // ret.a_dG2 = Rcpp::as<decltype(reta_dG2) >(xl["a_dG2"]);
  ret.a_dG2 = Rcpp::as<double >(xl["a_dG2"]);
  // ret.control = Rcpp::as<decltype(retcontrol) >(xl["control"]);
  ret.control = Rcpp::as<plant::Control >(xl["control"]);
  return ret;
}
template <> inline SEXP wrap(const plant::Internals& x) {
  Rcpp::List ret;
  ret["state_size"] = Rcpp::wrap(x.state_size);
  ret["states"] = Rcpp::wrap(x.states);
  ret["rates"] = Rcpp::wrap(x.rates);
  ret.attr("class") = "Internals";
  return ret;
}
template <> inline plant::Internals as(SEXP x) {
  if (!plant::RcppR6::is<plant::Internals >(x)) {
    Rcpp::stop("Expected an object of type Internals");
    // NOTE: Won't drop through or return anything.
  }
  // NOTE: assumes default constructable, and will assign *every*
  // field twice.  No current support for a hook.
  plant::Internals ret;
  Rcpp::List xl(x);
  // ret.state_size = Rcpp::as<decltype(retstate_size) >(xl["state_size"]);
  ret.state_size = Rcpp::as<int >(xl["state_size"]);
  // ret.states = Rcpp::as<decltype(retstates) >(xl["states"]);
  ret.states = Rcpp::as<std::vector<double> >(xl["states"]);
  // ret.rates = Rcpp::as<decltype(retrates) >(xl["rates"]);
  ret.rates = Rcpp::as<std::vector<double> >(xl["rates"]);
  return ret;
}
template <> inline SEXP wrap(const plant::Parameters<plant::FF16_Strategy>& x) {
  Rcpp::List ret;
  ret["k_I"] = Rcpp::wrap(x.k_I);
  ret["patch_area"] = Rcpp::wrap(x.patch_area);
  ret["n_patches"] = Rcpp::wrap(x.n_patches);
  ret["disturbance_mean_interval"] = Rcpp::wrap(x.disturbance_mean_interval);
  ret["strategies"] = Rcpp::wrap(x.strategies);
  ret["seed_rain"] = Rcpp::wrap(x.seed_rain);
  ret["is_resident"] = Rcpp::wrap(x.is_resident);
  ret["control"] = Rcpp::wrap(x.control);
  ret["strategy_default"] = Rcpp::wrap(x.strategy_default);
  ret["cohort_schedule_max_time"] = Rcpp::wrap(x.cohort_schedule_max_time);
  ret["cohort_schedule_times_default"] = Rcpp::wrap(x.cohort_schedule_times_default);
  ret["cohort_schedule_times"] = Rcpp::wrap(x.cohort_schedule_times);
  ret["cohort_schedule_ode_times"] = Rcpp::wrap(x.cohort_schedule_ode_times);
  ret["hyperpar"] = Rcpp::wrap(x.hyperpar);
  ret.attr("class") = Rcpp::CharacterVector::create("Parameters<FF16>", "Parameters");
  return ret;
}
template <> inline plant::Parameters<plant::FF16_Strategy> as(SEXP x) {
  if (!plant::RcppR6::is<plant::Parameters<plant::FF16_Strategy> >(x)) {
    Rcpp::stop("Expected an object of type Parameters<FF16>");
    // NOTE: Won't drop through or return anything.
  }
  // NOTE: assumes default constructable, and will assign *every*
  // field twice.  No current support for a hook.
  plant::Parameters<plant::FF16_Strategy> ret;
  Rcpp::List xl(x);
  // ret.k_I = Rcpp::as<decltype(retk_I) >(xl["k_I"]);
  ret.k_I = Rcpp::as<double >(xl["k_I"]);
  // ret.patch_area = Rcpp::as<decltype(retpatch_area) >(xl["patch_area"]);
  ret.patch_area = Rcpp::as<double >(xl["patch_area"]);
  // ret.n_patches = Rcpp::as<decltype(retn_patches) >(xl["n_patches"]);
  ret.n_patches = Rcpp::as<size_t >(xl["n_patches"]);
  // ret.disturbance_mean_interval = Rcpp::as<decltype(retdisturbance_mean_interval) >(xl["disturbance_mean_interval"]);
  ret.disturbance_mean_interval = Rcpp::as<double >(xl["disturbance_mean_interval"]);
  // ret.strategies = Rcpp::as<decltype(retstrategies) >(xl["strategies"]);
  ret.strategies = Rcpp::as<std::vector<plant::FF16_Strategy> >(xl["strategies"]);
  // ret.seed_rain = Rcpp::as<decltype(retseed_rain) >(xl["seed_rain"]);
  ret.seed_rain = Rcpp::as<std::vector<double> >(xl["seed_rain"]);
  // ret.is_resident = Rcpp::as<decltype(retis_resident) >(xl["is_resident"]);
  ret.is_resident = Rcpp::as<std::vector<bool> >(xl["is_resident"]);
  // ret.control = Rcpp::as<decltype(retcontrol) >(xl["control"]);
  ret.control = Rcpp::as<plant::Control >(xl["control"]);
  // ret.strategy_default = Rcpp::as<decltype(retstrategy_default) >(xl["strategy_default"]);
  ret.strategy_default = Rcpp::as<plant::FF16_Strategy >(xl["strategy_default"]);
  // ret.cohort_schedule_max_time = Rcpp::as<decltype(retcohort_schedule_max_time) >(xl["cohort_schedule_max_time"]);
  ret.cohort_schedule_max_time = Rcpp::as<double >(xl["cohort_schedule_max_time"]);
  // ret.cohort_schedule_times_default = Rcpp::as<decltype(retcohort_schedule_times_default) >(xl["cohort_schedule_times_default"]);
  ret.cohort_schedule_times_default = Rcpp::as<std::vector<double> >(xl["cohort_schedule_times_default"]);
  // ret.cohort_schedule_times = Rcpp::as<decltype(retcohort_schedule_times) >(xl["cohort_schedule_times"]);
  ret.cohort_schedule_times = Rcpp::as<std::vector<std::vector<double> > >(xl["cohort_schedule_times"]);
  // ret.cohort_schedule_ode_times = Rcpp::as<decltype(retcohort_schedule_ode_times) >(xl["cohort_schedule_ode_times"]);
  ret.cohort_schedule_ode_times = Rcpp::as<std::vector<double> >(xl["cohort_schedule_ode_times"]);
  // ret.hyperpar = Rcpp::as<decltype(rethyperpar) >(xl["hyperpar"]);
  ret.hyperpar = Rcpp::as<SEXP >(xl["hyperpar"]);
  ret.validate();
  return ret;
}
template <> inline SEXP wrap(const plant::Cohort<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::Cohort<plant::FF16_Strategy> >(x));
}
template <> inline plant::Cohort<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::Cohort<plant::FF16_Strategy> >(x));
}
template <> inline SEXP wrap(const plant::Species<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::Species<plant::FF16_Strategy> >(x));
}
template <> inline plant::Species<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::Species<plant::FF16_Strategy> >(x));
}
template <> inline SEXP wrap(const plant::Patch<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::Patch<plant::FF16_Strategy> >(x));
}
template <> inline plant::Patch<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::Patch<plant::FF16_Strategy> >(x));
}
template <> inline SEXP wrap(const plant::SCM<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::SCM<plant::FF16_Strategy> >(x));
}
template <> inline plant::SCM<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::SCM<plant::FF16_Strategy> >(x));
}
template <> inline SEXP wrap(const plant::StochasticSpecies<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::StochasticSpecies<plant::FF16_Strategy> >(x));
}
template <> inline plant::StochasticSpecies<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::StochasticSpecies<plant::FF16_Strategy> >(x));
}
template <> inline SEXP wrap(const plant::StochasticPatch<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::StochasticPatch<plant::FF16_Strategy> >(x));
}
template <> inline plant::StochasticPatch<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::StochasticPatch<plant::FF16_Strategy> >(x));
}
template <> inline SEXP wrap(const plant::StochasticPatchRunner<plant::FF16_Strategy>& x) {
  return wrap(plant::RcppR6::RcppR6<plant::StochasticPatchRunner<plant::FF16_Strategy> >(x));
}
template <> inline plant::StochasticPatchRunner<plant::FF16_Strategy> as(SEXP x) {
  return *(plant::RcppR6::RcppR6<plant::StochasticPatchRunner<plant::FF16_Strategy> >(x));
}
}

#endif
