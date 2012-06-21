use strict;
use warnings;

package Gentoo::Overlay::Group;

# ABSTRACT: A collection of Gentoo::Overlay objects.

use Moose;

use MooseX::Has::Sugar;
use MooseX::Types::Moose qw( :all );
use MooseX::Types::Path::Class qw( Dir );
use namespace::autoclean;

use Gentoo::Overlay v1.0.2;
use Gentoo::Overlay::Types qw( :all );
use Gentoo::Overlay::Exceptions qw( :all );
use Scalar::Util qw( blessed );

has '_overlays' => (
  ro, lazy,
  isa => HashRef [Gentoo__Overlay_Overlay],
  traits  => [qw( Hash )],
  default => sub { return {} },
  handles => {
    _has_overlay  => exists   =>,
    overlay_names => keys     =>,
    overlays      => elements =>,
    get_overlay   => get      =>,
    _set_overlay  => set      =>,
  },
);

my $_str = Str();

sub _type_print {
  ref $_         ? ref $_
    : defined $_ ? 'scalar<' . $_ . '>'
    : 'scalar=undef'

}

sub add_overlay {
  my ( $self, @args ) = @_;
  if ( @args == 1 and blessed $args[0] ) {
    goto $self->can('_add_overlay_object');
  }
  if ( $_str->check( $args[0] ) ) {
    goto $self->can('_add_overlay_string_path');
  }
  return exception(
    ident   => 'bad overlay type',
    message => qq{Unrecognised parameter types passed to add_overlay. Expected: \n%{signatures}s. Got: [%{type}s]},
    payload => {
      signatures => ( join q{},  map { qq{    \$group->add_overlay( $_ );\n} } qw( Str Path::Class::Dir Gentoo::Overlay ) ),
      type       => ( join q{,}, map { _type_print } @args ),
    }
  );
}

sub iterate {
  my ( $self, $what, $callback ) = @_;
  my %method_map = (
    ebuilds    => _iterate_ebuilds    =>,
    categories => _iterate_categories =>,
    packages   => _iterate_packages   =>,
    overlays   => _iterate_overlays   =>,
  );
  if ( exists $method_map{$what} ) {
    goto $self->can( $method_map{$what} );
  }
  return exception(
    ident   => 'bad iteration method',
    message => 'The iteration method %{what_method}s is not a known way to iterate.',
    payload => { what_method => $what, },
  );
}

sub _iterate_ebuilds {
  my ( $self, $what, $callback ) = @_;
  my $real_callback = sub {
    my (%package_config) = %{ $_[1] };
    my $inner_callback = sub {
      my (%ebuild_config) = %{ $_[1] };
      $self->$callback( { ( %package_config, %ebuild_config ) } );
    };
    $package_config{package}->_iterate_ebuilds( ebuilds => $inner_callback );
  };
  $self->_iterate_packages( packages => $real_callback );
  return;
}

# categories = { /overlays/categories

sub _iterate_categories {
  my ( $self, $what, $callback ) = @_;
  my $real_callback = sub {
    my (%overlay_config) = %{ $_[1] };
    my $inner_callback = sub {
      my (%category_config) = %{ $_[1] };
      $self->$callback( { ( %overlay_config, %category_config ) } );
    };
    $overlay_config{overlay}->_iterate_categories( categories => $inner_callback );
  };
  $self->_iterate_overlays( overlays => $real_callback );
  return;
}

sub _iterate_packages {
  my ( $self, $what, $callback ) = @_;
  my $real_callback = sub {
    my (%category_config) = %{ $_[1] };
    my $inner_callback = sub {
      my (%package_config) = %{ $_[1] };
      $self->$callback( { ( %category_config, %package_config ) } );
    };
    $category_config{category}->_iterate_packages( packages => $inner_callback );
  };
  $self->_iterate_categories( categories => $real_callback );
  return;
}

# overlays = { /overlays }
sub _iterate_overlays {
  my ( $self, $what, $callback ) = @_;
  my %overlays     = $self->overlays;
  my $num_overlays = scalar keys %overlays;
  my $last_overlay = $num_overlays - 1;
  my $offset       = 0;
  for my $overlay_name ( sort keys %overlays ) {
    local $_ = $overlays{$overlay_name};
    $self->$callback(
      {
        overlay_name => $overlay_name,
        overlay      => $overlays{$overlay_name},
        num_overlays => $num_overlays,
        last_overlay => $last_overlay,
        overlay_num  => $offset,
      }
    );
    $offset++;
  }
}

my $_gentoo_overlay = Gentoo__Overlay_Overlay();
my $_path_class_dir = Dir();

# This would be better in M:M:TypeCoercion

sub __can_corce {
  my ( $to_type, $from_thing ) = @_;
  if ( not defined $to_type->{_compiled_can_coerce} ) {
    my @coercion_map = @{ $to_type->type_coercion_map };
    my @coercions;
    while (@coercion_map) {
      my ( $constraint_name, $action ) = splice( @coercion_map, 0, 2 );
      my $type_constraint =
        ref $constraint_name ? $constraint_name : Moose::Util::TypeConstraints::find_or_parse_type_constraint($constraint_name);

      unless ( defined $type_constraint ) {
        require Moose;
        Moose->throw_error("Could not find the type constraint ($constraint_name) to coerce from");
      }

      push @coercions => [ $type_constraint->_compiled_type_constraint, $action ];
    }
    $to_type->{_compiled_can_coerce} = sub {
      my $thing = shift;
      foreach my $coercion (@coercions) {
        my ( $constraint, $converter ) = @$coercion;
        if ( $constraint->($thing) ) {
          return 1;
        }
      }
      return;
    };
  }
  return $to_type->{_compiled_can_coerce}->($from_thing);
}

sub _add_overlay_object {
  my ( $self, $object, @rest ) = @_;

  if ( $_gentoo_overlay->check($object) ) {
    goto $self->can('_add_overlay_gentoo_object');
  }
  if ( $_path_class_dir->check($object) ) {
    goto $self->can('_add_overlay_path_class');
  }
  return exception(
    ident   => 'bad overlay object type',
    message => qq{Unrecognised parameter object types passed to add_overlay. Expected: \n%{signatures}s. Got: [%{type}s]},
    payload => {
      signatures => ( join q{}, map { qq{    \$group->add_overlay( $_ );\n} } qw( Str Path::Class::Dir Gentoo::Overlay ) ),
      type => ( join q{,}, blessed $object, map { _type_print } @rest ),
    },
  );
}

sub _add_overlay_gentoo_object {
  my ( $self, $object, @rest ) = @_;
  $_gentoo_overlay->assert_valid($object);
  if ( $self->_has_overlay( $object->name ) ) {
    return exception(
      ident   => 'overlay exists',
      message => 'The overlay named %{overlay_name}s is already added to this group.',
      payload => { overlay_name => $object->name },
    );
  }
  $self->_set_overlay( $object->name, $object );
}

sub _add_overlay_path_class {
  my ( $self, $path, @rest ) = @_;
  $_path_class_dir->assert_valid($path);
  my $go = Gentoo::Overlay->new( path => $path, );
  @_ = ( $self, $go );
  goto $self->can('_add_overlay_gentoo_object');
}

sub _add_overlay_string_path {
  my ( $self, $path_str, @rest ) = @_;
  $_str->assert_valid($path_str);
  my $path = $_path_class_dir->coerce($path_str);
  @_ = ( $self, $path );
  goto $self->can('_add_overlay_path_class');
}

__PACKAGE__->meta->make_immutable;
no Moose;

1;
