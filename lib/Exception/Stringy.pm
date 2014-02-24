#
# This file is part of Exception-Stringy
#
# This software is Copyright (c) 2014 by Damien Krotkine.
#
# This is free software, licensed under:
#
#   The Artistic License 2.0 (GPL Compatible)
#
# ABSTRACT: a Perl Exceptions module where exceptions are not objects but simple strings.

package Exception::Stringy;
{
  $Exception::Stringy::VERSION = '0.10';
}
use strict;
use warnings;
use 5.10.0;

use Carp;

our @ISA = qw(Exporter);
our @EXPORT = qw($_x_throw $_x_rethrow $_x_raise $_x_class $_x_isa $_x_fields
                 $_x_field $_x_message $_x_error);


# regexp to extract header's type and flags
my $only_header_r = qr/(\[[^]|]+\|[^]]*\|\])/;
my $header_r = qr/\[([^]|]+)(\|([^]]*)\|)\]/;
my $klass_r  = qr/^([_a-zA-Z][_a-zA-Z0-9]*)$/;
my $field_name_r = qr/^([_a-zA-Z][_a-zA-Z0-9]*)$/;
my $field_value_r = qr/^([^\034:|]*)$/;
my $field_value_b64_r = qr|^\034([A-Za-z0-9+/=]+)$|;
my $is_b64 = qr|^(\034[A-Za-z0-9+/=]*)$|;

no strict 'refs';
no warnings qw(once);

my %registered;

use MIME::Base64;

sub _encode {
    $_[0] =~ $field_value_r
      and return $_[0];
    "\034" . encode_base64($_[0], '');
}

sub _decode {
    my ($t) = $_[0] =~ $field_value_b64_r
      or return $_[0];
    decode_base64($t);
}

sub import {
    my $class = shift;
    while ( scalar @_ ) {
        my $klass = shift;
        ($klass // '') =~ $klass_r or _croak(class => $klass);
        $registered{$klass} = 1;
        my $isa = $class;
        if (my $r = ref $_[0] ) {
            $r eq 'HASH' or _croak('exception definition structure (should be HASH)' => $r);
            my %h = %{shift()};
            %{"${klass}::Fields"} =
              map { $_ => 1 }
              my @f = map { ( ($_ // '') =~ $field_name_r)[0] // _croak(field => $_) }
              @{ $h{fields} };

            $h{isa} and $isa = $h{isa};

        }
        @{"${klass}::ISA"} = $isa;
    }
    $class->export_to_level(1);
}

            # foreach my $f (@f) {
            #     my $regexp = qr/\|$f:(.*?)\|/;
            #     *{"${klass}::$f"} = sub {
            #         my ($eklass, $fields) = $_[1] =~ $header_r
            #           or _croak(exception => $_[1]);
            #         @_ == 3
            #           or return ($fields =~ $regexp)[0];

            #         $fields =~ s/$regexp/|/;
            #         $fields =~ s/^\|\|$/|/;
            #         my $v = _encode($_[2]);
            #         my $was_ro = Internals::SvREADONLY($_[1]);
            #         Internals::SvREADONLY($_[1] => 0);
            #         $_[1] =~ s/$header_r/[$eklass$fields$f:$v|]/;
            #         Internals::SvREADONLY($_[1] => $was_ro);
            #         return $_[1];
            #     };
            # }


sub _croak { croak $_[0] . " '" . ($_[1] // '<undef>') . "' is invalid" }

# Class methods

sub new {
    my ($class, $message, %fields) = @_;
    $registered{$class} or croak "exception class '$class' has not been registered yet";
    '[' . $class . '|' . join('|',
      map  { $_ . ':' . _encode($fields{$_}) }
      grep { ${"${class}::Fields"}{$_}
             or croak "invalid field '$_', exception class '$class' didn't declare it"
           }
      keys %fields
    ) . '|]' . ($message // '');
}

sub raise { croak shift->new(@_)}
sub throw { croak shift->new(@_)}

sub registered_fields {
    my ($class) = @_;
    keys %{"${class}::Fields"};
}

sub registered_exception_classes { keys %registered }

# fake methods (class methods with exception as first argument)

our $_x_throw   = sub { croak $_[0] };
our $_x_rethrow = sub { croak $_[0] };
our $_x_raise   = sub { croak $_[0] };

our $_x_class = sub {
    my ($class) = $_[0] =~ $header_r
      or _croak(exception => $_[0]);
    $class;
};

our $_x_isa = sub {
    my ($class) = $_[0] =~ $header_r
      or _croak(exception => $_[0]);
    $class->isa($_[1]);
};

our $_x_fields = sub {
    my ($class, $fields) = $_[0] =~ $header_r
      or _croak(exception => $_[0]);
    map { (split(/:/, $_))[0] } split(/\|/, $fields);
};

our $_x_field = sub {
    my $f = $_[1];
    my ($class, $fields) = $_[0] =~ $header_r
      or _croak(exception => $_[0]);
    my $regexp = qr/\|$f:(.*?)\|/;
    ${"${class}::Fields"}{$f}
      or _croak("Unknown field for this exception class ('$class'): " => $f);
    @_ == 3
      or return _decode( ($fields =~ $regexp)[0] // return );

    $fields =~ s/$regexp/|/;
    $fields =~ s/^\|\|$/|/;
    my $v = _encode($_[2]);
    my $was_ro = Internals::SvREADONLY($_[0]);
    Internals::SvREADONLY($_[0] => 0);
    $_[0] =~ s/$header_r/[$class$fields$f:$v|]/;
    Internals::SvREADONLY($_[0] => $was_ro);
    return;
};

our $_x_message = sub {
    @_ == 2
      or return( $_[0] =~ s/$only_header_r//r
                 or _croak(exception => $_[0])
               );

    my ($header) = $_[0] =~ $only_header_r
      or _croak(exception => $_[0]);
    my $was_ro = Internals::SvREADONLY($_[0]);
    Internals::SvREADONLY($_[0] => 0);
    $_[0] = "$header$_[1]";
    Internals::SvREADONLY($_[0] => $was_ro);
    return $_[0];
};

our $_x_error = $_x_message;



1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Exception::Stringy - a Perl Exceptions module where exceptions are not objects but simple strings.

=head1 VERSION

version 0.10

=head1 SYNOPSIS

  use Exception::Stringy (
      'MyException',
   
      'YetAnotherException' => {
          isa         => 'AnotherException',
      },
   
      'ExceptionWithFields' => {
          isa    => 'YetAnotherException',
          fields => [ 'grandiosity', 'quixotic' ],
          alias  => 'throw_fields',
      },
  );
  
  ## with Try::Tiny
  
  use Try::Tiny;
   
  try {
      # throw an exception
      MyException->throw('I feel funny.');
  
      # or use an aliase
      throw_fields 'Error message', grandiosity => 1;

      # you can build exception step by step
      my $e = ExceptionWithFields->new("The error message");
      $e->$_x_field(quixotic => "some_value");
      $e->$_x_throw();
  
  }
  catch {
      if ( $_->$_x_isa('Exception::Stringy') ) {
          warn $_->$_x_error, "\n";
      }
  
      if ( $_->$_x_isa('ExceptionWithFields') ) {
          if ( $e->_x_field('quixotic') ) {
              handle_quixotic_exception();
          }
          else {
              handle_non_quixotic_exception();
          }
      }
      else {
          $_->$_x_rethrow;
      }
  };
   
  # without Try::Tiny
   
  eval {
      # ...
      MyException->throw('I feel funny.');
      1;
  } or do {
      my $e = $@;
      # .. same as above with $e instead of $_
  }

=head1 DESCRIPTION

This module allows you to declare exceptions, and provides a simple interface
to declare, throw, and interact with them. It can be seen as a light version of
C<Exception::Class>, except that there is a catch: exceptions are B<not
objects>, they are B<normal strings>, with a pattern that contains properties.

=head1 WHY WOULD YOU WANT SUCH THING ?

Having exceptions be objects is sometimes very annoying. What if some code is
calling you, and isn't expecting objects exceptions ? Sometimes string
overloading doesn't work. Sometimes, external code tamper with your exception.
Consider:

  use Exception::Class ('MyException');
  use Scalar::Util qw( blessed );
  use Try::Tiny;

  $SIG{__DIE__} = sub { die "FATAL: $_[0]" };

  try {
    MyException->throw("foo");
  } catch {
    die "this is not a Class::Exception" unless blessed $_ && $_->can('rethrow');
    if ($_->isa('MyException')) { ... }
  };

In this example, the exception thrown is a C<Class::Exception> instance, but it
gets forced to a string by the signal handler. When in the catch block, it's
not an object, it's a regular string, and the code fails to see that it's a
'MyException'.

=head1 BUT THIS NEVER HAPPENS

Well, don't use this module then :)

=head1 BASIC USAGE

=head2 Declaring exception types

=head2 throwing exceptions

  ExceptionWithFields->throw("error message", grandiosity => 42);

=head2 catching and checking exceptions

  eval { ... 1; } or do {
    my $e = $@;
    if ($e->$_x_isa('Exception::Stringy')) {
      if ($e->$_x_isa('ExceptionWithFields')) {
        ...
      } elsif ($e->$_x_isa('YetAnotherException')) {
        ...
      }
    } else {
      # this works on anything, even objects or bare strings
      e->$_x_rethrow;
    }
  };

=head1 CLASS METHODS

=head2 raise, throw

  # both are exactly the same
  ExceptionWithFields->throw("error message", grandiosity => 42);
  ExceptionWithFields->raise("error message", grandiosity => 42);

Creates an string exception from the given class, with the error message and
fields, then throws the exception. The exception is thrown using C<croak()>
from the C<Carp> module.

The error message is always the first argument If ommited, it'll default to
empty string. Optional fields are provided as flat key / value pairs.

=head2 new

  my $e = ExceptionWithFields->new("error message", grandiosity => 42);

Takes the same arguments as C<throw()> but doesn't throw the exception.
Instead, the exception is returned.

=head2 registered_fields 

  my @fields = ExceptionWithFields->registered_fields;

Returns the possible fields that an exception of the given class can have.

=head2 registered_exception_classes

  my @class_names = Exception::Stringy->registered_exception_classes;

Returns the exceptions classes that have been registered.

=head1 METHODS

The syntax is a bit strange, but that's because exceptions are bare strings,
and not blessed references, so we have to use a trick to have the arrow syntax
working ( thanks to pokki for the hint ).

The _x_ prefix is used because C<x> looks like C<exception>. 

=head2 $_x_throw(), $_x_rethrow(), $_x_raise()

  $exception->$_x_throw();
  $exception->$_x_rethrow();
  $exception->$_x_raise();

Throws the exception.

=head2 $_x_class()

  my $class = $exception->$_x_class();

Returns the exception class name.

=head2 $_x_isa()

  if ($exception->$_x_isa('ExceptionClass')) {... }

Returns true if the class of the given exception C<->isa()> the class given in
parameter.

=head2 $_x_fields()

  my @fields = $exception->$_x_fields();

Returns the list of field names that are in the exception.

=head2 $_x_field()

  my $value = $exception->$_x_field('field_name');

  $exception->$_x_field(field_name => $value);

Set or get the given field. If the value contains one of these forbidden
caracters, then it is transparently base64 encoded and decoded.

The list of forbidden caracters are:

=over

=item C<:>

the semicolon

=item C<|>

the pipe

=item C<\034>

C<\034>, the 0x28 seperator ASCII caracter.

=back

=head2 $_x_message(), $_x_error()

  my $text = $exception->$_x_message();
  my $text = $exception->$_x_error();

  $exception->$_x_message("Error message");
  $exception->$_x_error("Error message");

Set or get the error message of the exception

=head1 AUTHOR

Damien Krotkine <dams@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is Copyright (c) 2014 by Damien Krotkine.

This is free software, licensed under:

  The Artistic License 2.0 (GPL Compatible)

=cut
